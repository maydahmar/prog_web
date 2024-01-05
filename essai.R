library(shiny)
library(shinydashboard)
library(FactoMineR)
library(factoextra)
library(rpart)

# Fonction de normalisation
normalizeData <- function(column) {
  (column - min(column)) / (max(column) - min(column))
}

# Fonction de dummification
dummifyData <- function(df) {
  as.data.frame(lapply(df, function(column) {
    if (is.factor(column) || is.character(column)) {
      model.matrix(~ column - 1)[, -1]
    } else {
      column
    }
  }))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analyse Avancée de Données"),
  dashboardSidebar(
    fileInput("file1", "Choisir un fichier", accept = c(".csv", ".dat", ".txt",".data")),
    actionButton("load", "Charger les données"),
    checkboxInput("normalize", "Normaliser les données", value = FALSE),
    checkboxInput("dummy", "Dummifier les données", value = FALSE),
    selectInput("targetVariable", "Variable Cible", choices = NULL)
  ),
  dashboardBody(
    tabsetPanel(
      id = "mainTabset",
      tabPanel("Données",
               fluidRow(
                 uiOutput("dataSummaryUI")
               ),
               fluidRow(
                 uiOutput("missingDataOptionsUI")
               )
      ),
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Déséquilibre des Classes", plotOutput("classImbalance")),
      tabPanel("ACP Visualisation",
               radioButtons("visualizationType", "Correlation entre les :",
                            choices = c("Individus" = "ind", "Variables" = "var"), selected = "VAR"),
               sliderInput("numObservations", "Nombre d'observations à inclure dans l'ACP:",
                           min = 1, max = 3750, value = 100, step = 1),
               plotOutput("visualisationPlot")
      ),
      tabPanel("Modèles de Classification",
               fluidRow(
                 box(title = "Entraînement des Modèles",
                     selectInput("selectedModel", "Choisir le Modèle",
                                 choices = c("Régression Logistique" = "lm", "Arbre de Décision" = "rpart")),
                     actionButton("trainModels", "Entraîner le Modèle")
                 ),
                 box(title = "Évaluation Comparative",
                     plotOutput("modelComparisonPlot")),
                 box(title = "Résultats",
                     verbatimTextOutput("modelResultsText"))
               )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values for data and missing data info
  values <- reactiveValues(data = NULL, missingInfo = NULL, acp_result = NULL, afcm_result = NULL)
  
  observeEvent(input$load, {
    req(input$file1)
    
    df <- read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    if (input$dummy) {
      df <- dummifyData(df)
    }
    
    if (input$normalize) {
      df <- as.data.frame(lapply(df, normalizeData))
    }
    
    # Store the data in a reactive value
    values$data <- df
    
    # Update the select input for the target variable
    updateSelectInput(session, "targetVariable", choices = names(df))
    
    # Identifier les variables numériques
    values$numeric_vars <- sapply(df, function(column) is.numeric(column))
    
    # Extraire les colonnes des variables numériques
    values$df_numeric <- df[values$numeric_vars]
    
    
    # Calcul de l'ACP
    values$acp_result <- PCA(values$df_numeric, graph = FALSE)
    
    # Conversion des variables qualitatives en variables factices pour l'AFDM
    #df_afcm <- dummifyData(df)
    
    # Identifier les variables catégorielles
    values$categorical_vars <- sapply(df, function(column) is.character(column) || is.factor(column))
    
    # Extraire les colonnes des variables catégorielles
    values$df_categ <- df[values$categorical_vars]
    
    # Calcul de l'AFDM
    values$afcm_result <- MCA(values$df_categ, graph = FALSE)
  })
  
  output$dataSummaryUI <- renderUI({
    req(values$data)
    
    # Séparation des variables quantitatives et qualitatives
    quant_vars <- Filter(is.numeric, values$data)
    qual_vars <- Filter(is.character, values$data)
    
    # Affichage des résumés pour les variables quantitatives
    quant_summary <- lapply(names(quant_vars), function(var) {
      first_few <- head(values$data[[var]], 3)
      list(QuantitativeVariable = var, FirstFewValues = first_few)
    })
    
    # Affichage des résumés pour les variables qualitatives
    qual_summary <- lapply(names(qual_vars), function(var) {
      first_few <- head(values$data[[var]], 3)
      list(QualitativeVariable = var, FirstFewValues = first_few)
    })
    
    # Affichage des valeurs manquantes
    missing_info <- sapply(values$data, function(x) sum(is.na(x)))
    missing_data_summary <- paste("Valeurs manquantes par variable:\n", toString(missing_info))
    
    # Création de l'UI output pour les résumés
    tagList(
      box(title = "Variables Quantitatives", width = 6, status = "primary", solidHeader = TRUE,
          verbatimTextOutput("quantitativeSummaryText")),
      box(title = "Variables Qualitatives", width = 6, status = "primary", solidHeader = TRUE,
          verbatimTextOutput("qualitativeSummaryText")),
      box(title = "Valeurs manquantes", width = 12, status = "primary", solidHeader = TRUE,
          verbatimTextOutput("missingDataText"))
    )
  })
  
  output$quantitativeSummaryText <- renderText({
    req(values$data)
    quant_vars <- Filter(is.numeric, values$data)
    # Affichage des résumés pour les variables quantitatives
    quant_summary <- lapply(names(quant_vars), function(var) {
      first_few <- head(values$data[[var]], 3)
      paste("Variable:", var, "\n", "Premières valeurs:", toString(first_few), "\n\n")
    })
    paste(quant_summary, collapse = "\n")
  })
  
  output$qualitativeSummaryText <- renderText({
    req(values$data)
    qual_vars <- Filter(is.character, values$data)
    # Affichage des résumés pour les variables qualitatives
    qual_summary <- lapply(names(qual_vars), function(var) {
      first_few <- head(values$data[[var]], 3)
      paste("Variable:", var, "\n", "Premières valeurs:", toString(first_few), "\n\n")
    })
    paste(qual_summary, collapse = "\n")
  })
  
  output$missingDataText <- renderText({
    req(values$data)
    missing_info <- sapply(values$data, function(x) sum(is.na(x)))
    paste("Valeurs manquantes par variable:\n", toString(missing_info))
  })
  
  output$missingDataOptionsUI <- renderUI({
    req(values$data)
    # UI for missing data treatment options
    tagList(
      radioButtons("missingDataTreatment", "Traitement des valeurs manquantes:",
                   choices = c("Remplacer par la moyenne" = "mean", "Supprimer" = "omit")),
      actionButton("applyTreatment", "Appliquer le traitement")
    )
  })
  
  observeEvent(input$applyTreatment, {
    req(values$data)
    treatment <- input$missingDataTreatment
    for (varName in names(values$missingInfo)) {
      if (values$missingInfo[varName] > 0 && treatment == "mean") {
        if (is.numeric(values$data[[varName]])) {
          # Replace missing values with the mean for numeric variables
          values$data[[varName]][is.na(values$data[[varName]])] <- mean(values$data[[varName]], na.rm = TRUE)
          cat('tot1')
        } else if (is.character(values$data[[varName]])) {
          # Replace missing values with the mode for character variables
          values$data[[varName]][is.na(values$data[[varName]])] <- Mode(values$data[[varName]])
          cat('toto2')
        }
      }
    }
    
    # Update the missingInfo after treatment
    values$missingInfo <- sapply(values$data, function(x) sum(is.na(x)))
  })
  
  output$visualisationPlot <- renderPlot({
    req(values$data, input$visualizationType)
    
    if (input$visualizationType == "var") {
      # Sélection des premières observations en fonction du slider
      df_subset <- values$df_numeric[1:input$numObservations, ]
      
      # Calcul de l'ACP
      acp_result <- PCA(df_subset, graph = FALSE)
      
      # Affichage du cercle des corrélations pour l'ACP sur les deux premières dimensions
      fviz_pca_var(acp_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, title = "ACP - Variables")
    } else if (input$visualizationType == "ind") {
      # Sélection des premières observations en fonction du slider
      df_subset <- values$df_numeric[1:input$numObservations, ]
      
      # Calcul de l'ACP
      acp_result <- PCA(df_subset, graph = FALSE)
      
      # Affichage du cercle des corrélations pour l'ACP sur les deux premières dimensions
      fviz_pca_ind(acp_result, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, title = "ACP - Individus")
    }
  })
  
  # Reactive values for training models
  trained_model <- reactiveValues()
  
  observeEvent(input$trainModels, {
    req(input$file1, input$targetVariable, input$selectedModel)
    
    # Séparer les données en variables explicatives (X) et variable cible (Y)
    X_train <- values$df_numeric[, -which(names(values$df_numeric) == input$targetVariable, arr.ind = TRUE)]
    Y_train <- values$df_numeric[, input$targetVariable]
    
    print(Y_train)
    
    # Entraîner le modèle sélectionné
    if (input$selectedModel == "lm") {
      model <- lm(Y_train ~ ., data = values$df_numeric)
    } else if (input$selectedModel == "rpart") {
      model <- rpart(Y_train ~ ., data = values$df_numeric, method = "anova")
    }
    
    
    # Stocker le modèle entraîné dans reactiveValues
    trained_model$model <- model
  })
  
  # Comparaison des modèles
  output$modelComparisonPlot <- renderPlot({
    req(trained_model$model)
    
    # Ajoutez votre logique pour la comparaison des modèles ici
    # Vous pouvez utiliser des métriques comme la précision, le rappel, le F-score, ROC, AUC, etc.
    # Exemple simplifié:
    results <- confusionMatrix(predict(trained_model$model, newdata = X_train, type = "response"), Y_train)
    plot(results)
  })
  
  # Afficher les résultats
  output$modelResultsText <- renderText({
    req(trained_model$model)
    
    # Ajoutez votre logique pour afficher les résultats ici
    # Vous pouvez extraire les performances du modèle et afficher les métriques souhaitées
    # Exemple simplifié:
    results <- confusionMatrix(predict(trained_model$model, newdata = X_train, type = "response"), Y_train)
    paste("Résultats de l'Entraînement du Modèle:\n", results)
  })
}


shinyApp(ui, server)
