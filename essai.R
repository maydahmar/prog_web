library(shiny)
library(shinydashboard)
library(FactoMineR)
library(factoextra)
library(rpart)
library(caret)
library(randomForest)



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
                 column(6,
                        box(title = "Entraînement des Modèles",
                            selectInput("selectedModel", "Choisir le Modèle",
                                        choices = c("Régression Linéaire" = "lm", 
                                                    "Arbre de Décision" = "rpart", 
                                                    "Random Forest" = "rf",
                                                    "Régression Ridge" = "ridge")),
                            actionButton("trainModels", "Entraîner le Modèle")
                        )
                 ),
                 column(6,
                        box(title = "Évaluation Comparative",
                            plotOutput("modelComparisonPlot"))
                 )
               ),
               fluidRow(
                 column(12,
                        box(title = "Résultats",
                            verbatimTextOutput("modelResultsText"),
                            plotOutput("scatterPlot")
                        )
                 )
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
    
    
    values$df_dummified <- dummifyData(df)
    
    # Calcul de l'ACP
    values$acp_result <- PCA(values$df_dummified, graph = FALSE)
    
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
        } else if (is.character(values$data[[varName]])) {
          # Replace missing values with the mode for character variables
          values$data[[varName]][is.na(values$data[[varName]])] <- Mode(values$data[[varName]])
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
  trained_model <- reactiveValues(model = NULL, lm = NULL, rpart = NULL, rf = NULL, lr = NULL)
  test <- reactiveValues(X_test = NULL, Y_test = NULL)
  
  
  observeEvent(input$trainModels, {
    req(values$data, input$file1, input$targetVariable, input$selectedModel)
    # Séparer les données en variables explicatives (X) et variable cible (Y)
    X <- values$df_numeric[, -which(names(values$df_numeric) == input$targetVariable, arr.ind = TRUE)]
    Y <- values$df_numeric[, input$targetVariable]
    
    # Diviser les données en ensembles d'entraînement et de test (80% train, 20% test)
    set.seed(123)  # Pour la reproductibilité
    
    splitIndex <- createDataPartition(Y, p = 0.8, list = FALSE)
    X_train <- X[splitIndex, ]
    Y_train <- Y[splitIndex]
    X_test <- X[-splitIndex, ]
    Y_test <- Y[-splitIndex]
    
    # Entraîner le modèle sélectionné
    if (input$selectedModel == "lm") {
      model <- lm(Y_train ~ ., data = X_train)
      trained_model$lm <- model
    } else if (input$selectedModel == "rpart") {
      model <- rpart(Y_train ~ ., data = X_train, method = "anova")
      trained_model$rpart <- model
    } else if (input$selectedModel == "rf") {
      # Utiliser un modèle de forêt aléatoire
      model <- randomForest(X_train, Y_train)
      trained_model$rf <- model
    } else if (input$selectedModel == "ridge") {
      # Utiliser un modèle de régression ridge
      model <- train(X_train, Y_train, method = "ridge")
      trained_model$lr <- model
    }
    
    # Stocker le modèle entraîné dans reactiveValues
    trained_model$model <- model
    
    # Stocker les données de test dans reactiveValues
    test$X_test <- X_test
    test$Y_test <- Y_test
  })
  
  
  # Comparaison des modèles
  output$modelComparisonPlot <- renderPlot({
    req(trained_model, test)
    
    # Liste pour stocker les métriques de performance de chaque modèle
    metrics_list <- list()
    
    # Fonction pour calculer les métriques et les stocker dans la liste
    calculate_metrics <- function(model, X_test, Y_test) {
      predictions <- predict(model, newdata = X_test)
      rmse <- sqrt(mean((Y_test - predictions)^2))
      mae <- mean(abs(Y_test - predictions))
      return(c(RMSE = rmse, MAE = mae))
    }
    
    # Calculer les métriques pour chaque modèle et les stocker dans la liste
    metrics_list[["Régression Linéaire"]] <- calculate_metrics(trained_model$lm, test$X_test, test$Y_test)
    metrics_list[["Arbre de Décision"]] <- calculate_metrics(trained_model$rpart, test$X_test, test$Y_test)
    metrics_list[["Random Forest"]] <- calculate_metrics(trained_model$rf, test$X_test, test$Y_test)
    metrics_list[["Régression Ridge"]] <- calculate_metrics(trained_model$lr, test$X_test, test$Y_test)
    
    # Convertir la liste en un dataframe pour faciliter la manipulation des données
    metrics_df <- as.data.frame(metrics_list)
    
    # Tracer un graphique à barres
    barplot(t(metrics_df), beside = TRUE, col = rainbow(4), legend.text = rownames(metrics_df),
            main = "Comparaison des Modèles", ylab = "Scores", xlab = "Métriques", ylim = c(0, max(unlist(metrics_list)) + 1))
    legend("topright", legend = colnames(metrics_df), fill = rainbow(4))
  })
  
  # Afficher les résultats
  output$modelResultsText <- renderText({
    req(trained_model$model, test)
    
    results <- data.frame(
      Observed = test$Y_test,
      Predicted = predict(trained_model$model, newdata = test$X_test)
    )
    
    rmse <- sqrt(mean((results$Observed - results$Predicted)^2))
    mae <- mean(abs(results$Observed - results$Predicted))
    
    paste("Résultats de l'Entraînement du Modèle:\n", 
          "RMSE:", rmse, "\n",
          "MAE:", mae)
  })
  
  output$scatterPlot <- renderPlot({
    req(trained_model$model, test$X_test, test$Y_test)
    
    predictions <- predict(trained_model$model, newdata = test$X_test)
    
    # Créer un nuage de points
    plot(test$Y_test, predictions, main = "Nuage de points entre valeurs réelles et prédites",
         xlab = "Valeurs réelles", ylab = "Prédictions")
    
    # Ajouter une ligne de référence pour une prédiction parfaite
    abline(a = 0, b = 1, col = "red")
  })
  
}


shinyApp(ui, server)
