library(DT)
library(shiny)
library(shinydashboard)
library(FactoMineR)
library(factoextra)
library(rpart)
library(caret)
library(randomForest)
library(tools)
library(magrittr)
library(shinyjs)
library(plotly)
library(kernlab)
library(e1071)
library(pROC)
library(dplyr)


# Fonction pour calculer le mode
Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Fonction de normalisation
normalizeData <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    } else {
      x
    }
  })
  return(df)
}

# Modifier la fonction dummifyData pour inclure les caractères
dummifyData <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      dummy_matrix <- model.matrix(~ x - 1)
      dummy_matrix <- dummy_matrix[, -1]  # Remove intercept
      colnames(dummy_matrix) <- make.names(colnames(dummy_matrix))
      return(as.data.frame(dummy_matrix))
    } else {
      return(x)
    }
  })
  
  # Combine all dummy data frames
  df_dummified <- do.call(cbind, df)
  
  # Return the modified data frame
  return(df_dummified)
}



# Fonction pour imputer les données manquantes selon le type
imputeData <- function(df, quantitative_method, qualitative_method) {
  for (col_name in names(df)) {
    if (is.numeric(df[[col_name]])) {
      if (quantitative_method == "mean") {
        df[[col_name]][is.na(df[[col_name]])] <- mean(df[[col_name]], na.rm = TRUE)
      } else if (quantitative_method == "median") {
        df[[col_name]][is.na(df[[col_name]])] <- median(df[[col_name]], na.rm = TRUE)
      } else if (quantitative_method == "mode") {
        df[[col_name]][is.na(df[[col_name]])] <- Mode(df[[col_name]])
      }
    } else {
      if (qualitative_method == "mode") {
        df[[col_name]][is.na(df[[col_name]])] <- Mode(df[[col_name]])
      } else if (qualitative_method == "new_category") {
        df[[col_name]][is.na(df[[col_name]])] <- "Inconnu"
      }
    }
  }
  return(df)
}

# Fonction pour calculer et retourner les détails des valeurs manquantes
calculateMissingDetails <- function(df) {
  na_count <- sapply(df, function(x) sum(is.na(x)))
  categories_count <- sapply(df, function(x) if (is.factor(x) || is.character(x)) length(unique(na.omit(x))) else NA)
  categories_list <- sapply(df, function(x) if (is.factor(x) || is.character(x)) paste(unique(na.omit(x)), collapse=", ") else NA)
  
  na_detail <- data.frame(
    Variable = names(na_count),
    MissingCount = na_count,
    MissingPercent = (na_count / nrow(df)) * 100,
    Type = sapply(df, function(x) {
      if (is.numeric(x)) "Quantitative" else "Qualitative"
    }),
    Categories = categories_count, # Nombre de catégories
    CategoriesList = categories_list # Liste des catégories
  )
  na_detail
}


# Fonction pour détecter les outliers
detectOutliers <- function(df) {
  # Créer un data frame pour stocker les outliers
  outliers_df <- data.frame(
    Variable = character(),
    OutlierValue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Pour chaque variable quantitative, calculer les outliers
  for (var in names(df)) {
    if (is.numeric(df[[var]])) {
      Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      outliers <- df[[var]][df[[var]] < (Q1 - 1.5 * IQR) | df[[var]] > (Q3 + 1.5 * IQR)]
      if (length(outliers) > 0) {
        outliers_df <- rbind(outliers_df, data.frame(
          Variable = var,
          OutlierValue = outliers,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  return(outliers_df)
}

# Fonction pour afficher la matrice de confusion avec plotly
plot_confusion_matrix <- function(confusion, title) {
  plot_ly(
    x = confusion$positive,
    y = confusion$Reference,
    z = confusion$table,
    type = "heatmap",
    colorscale = "Viridis",
    showscale = TRUE
  ) %>%
    layout(
      title = title,
      xaxis = list(title = "Prédictions", ticks = "outside"),
      yaxis = list(title = "Référence", ticks = "outside")
    )
}

# Fonction pour calculer les métriques et les stocker dans la liste
calculate_metrics <- function(model, X_test, Y_test) {
  predictions <- predict(model, newdata = X_test)
  rmse <- sqrt(mean((Y_test - predictions)^2))
  mae <- mean(abs(Y_test - predictions))
  return(c(RMSE = rmse, MAE = mae))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analyse Avancée de Données"),
  dashboardSidebar(
    fileInput("file1", "Choisir un fichier", accept = c(".csv", ".dat", ".txt", ".data", ".xls", ".xlsx" )),
    actionButton("load", "Charger les données"),
    checkboxInput("normalize", "Normaliser les données"),
    checkboxInput("dummy", "Dummifier les données")
  ),
  dashboardBody(
    tabsetPanel(id = "tabs",
                tabPanel("Données", value = "data_panel",
                         fluidRow(
                           
                           box(title = "Dataset", status = "warning", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_data", "Afficher")),
                           
                           box(title = "Outliers", status = "danger", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_outliers", "Détails")),
                           
                           box(title = "Colonne Constante", status = "info", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_constant", "Détails")),
                           
                           box(title = "Variables manquantes", status = "primary", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_missing", "Détails")),
                           
                           
                           
                           
                         ),
                         uiOutput("dynamicTableUI") 
                ),
                tabPanel("Données Dummifiées", 
                         DTOutput("tableDummified")),
                
                tabPanel("Plot", value = "plot_panel", plotOutput("dataPlot")),
                tabPanel("Déséquilibre des Classes",
                         DTOutput("tableVariableTypes")
                ),
                tabPanel("ACP Visualisation",
                         radioButtons("visualizationType", "Correlation entre les :",
                                      choices = c("Individus" = "ind", "Variables" = "var"), selected = "VAR"),
                         sliderInput("numObservations", "Nombre d'observations à inclure dans l'ACP:",
                                     min = 1, max = 3750, value = 100, step = 1),
                         plotOutput("visualisationPlot")
                ),
                tabPanel("Classification",
                         fluidRow(
                           h2("Performance metric"),
                           selectInput("targetVariable", "Variable Cible", choices = NULL),
                           actionButton("trainClassification", "Entraîner les Modèles"),
                           h2("AUC Curve"),
                           fluidRow(
                             box(
                               title = "SVM", height = "470px", width = 5,
                               plotOutput("rocSvm") 
                             ),
                             box(
                               title ="Random Forest", height = "470px", width = 5,
                               plotOutput("rocTree") 
                             )
                           ),
                           h2("Confusion matrix"),
                           fluidRow(
                             box(
                               title = "SVM", height = "470px", width = 5,
                               plotlyOutput("confusionSVM") 
                             ),
                             box(
                               title = "DecisonTree", height = "470px", width = 5,
                               plotlyOutput("confusionTree") 
                             )
                           )
                         )
                ),
                tabPanel("Regression",
                         fluidRow(
                           h2(" Performance metric"),
                           selectInput("targetVariablereg", "Variable Cible", choices = NULL),
                           actionButton("trainRegression", "Entraîner les Modèles"),
                           h2(" Graphique des Résidus"),
                           fluidRow(
                             box(
                               title = "Régression Linéaire", height = "470px", width = 4,
                               plotOutput("reslm") 
                             ),
                             box(
                               title ="Arbre de Décision", height = "470px", width = 4,
                               plotOutput("resrpart") 
                             )
                           )
                           #fluidRow(
                           #  box(
                           #    title = "Random Forest", height = "470px", width = 4,
                           #    plotlyOutput("resrf") 
                           #  ),
                           #  box(
                           #    title = "Régression Ridge", height = "470px", width = 4,
                           #    plotlyOutput("resrg") 
                           #  )
                           #)
                         ),
                         fluidRow(
                           h2(" Comparaison des métriques"),
                           box(
                             height = "470px", width = 4,
                             plotOutput("metriquesreg") 
                           )
                         )
                )
    )
  )
)
# Server
server <- function(input, output, session) {
  dataOriginal <- reactiveVal(NULL)
  dataProcessed <- reactiveVal(NULL)
  reactiveOutliers <- reactiveVal(NULL)
  reactiveConstantColumns <- reactiveVal(NULL)
  
  observeEvent(input$load, {
    inFile <- input$file1
    if (is.null(inFile)) {return(NULL)}
    
    ext <- tools::file_ext(inFile$datapath)
    df <- switch(ext,
                 csv = { read.csv(inFile$datapath) },
                 dat = { read.table(inFile$datapath, col.names = c('age', 'sexe', 'chest pain type', 'resting blood pressure', 'serum cholestoral', 'fasting_blood_sugar', 'resting electrocardiographic results','maximum heart rate achieved','exercise induced angina','oldpeak', 'slope of the peak', 'major vessels', 'thal', 'absence')) },
                 txt = { read.delim(inFile$datapath) },
                 data = read.table(inFile$datapath, header = TRUE, sep = ","),
                 stop("Type de fichier non supporté")
    )
    
    df<- cbind(df, NouvelleColonne = 1.0)
    # Attribuer un titre à la nouvelle colonne
    colnames(df)[ncol(df)] <- "cst"
    
    # Supposons que "MaVariable" est la colonne dans laquelle vous voulez introduire des valeurs manquantes
    set.seed(123)  # Pour rendre les résultats reproductibles
    #df <- mutate(df, age = ifelse(runif(n()) < 0.1, NA, age))
    
    
    # Update the select input for the target variable
    # Mettre à jour le select input avec la dernière colonne par défaut
    last_column_name <- tail(names(df), 1)
    updateSelectInput(session, "targetVariable", choices = names(df), selected = last_column_name)
    
    updateSelectInput(session, "targetVariablereg", choices = names(df), selected = last_column_name)
    
    
    dataOriginal(df)
    dataProcessed(df)
  })
  
  
  observeEvent(input$dummy, {
    req(input$dummy)  # Vérifie si la case est cochée
    df <- dataOriginal()  # Récupère les données originales
    df_dummified <- dummifyData(df)  # Dummifie les données
    dataProcessed(df_dummified)  # Met à jour la valeur réactive avec les données dummifiées
    
    # Maintenant, affichez les données dummifiées dans la table
    output$tableDummified <- renderDataTable({
      req(dataProcessed())  # Assurez-vous que les données sont chargées
      dataProcessed()  # Renvoie les données dummifiées pour l'affichage
    })
  })
   
  # Appliquer ou annuler la normalisation et la dummification
  observe({
    req(dataOriginal())
    df <- dataOriginal()
    
    if (input$normalize) {
      df <- normalizeData(df)
    }
    
    #if (input$dummy) {
     # df <- dummifyData(df)
   # }
    
    
    
    
    dataProcessed(df)
  })
  
  
  # Utilisez une valeur réactive pour contrôler quel tableau est affiché
  currentView <- reactiveVal(NULL)
  
  observeEvent(input$show_missing, {
    currentView("missing")
    
  })
  
  # Observateur pour le bouton d'affichage des données
  observeEvent(input$show_data, {
    # Utiliser la valeur réactive pour afficher les données
    currentView("data")
  })
  
  
  observeEvent(input$show_outliers, {
    currentView("outliers")
    
  })
  
  
  output$dynamicTableUI <- renderUI({
    if(currentView() == "missing") {
      DTOutput("missingDetailsTable")
    } else if(currentView() == "outliers") {
      {
        tagList(
          DTOutput("outliersTable"),
          hr(),
          selectInput("outlier_action", "Action on outliers:", choices = c("None", "Remove")),
          actionButton("apply_outlier_action", "Apply Action")
        )
      } 
    }
    else if(currentView() == "data") {
      DTOutput("dataTable")
    }
  })
  
  
  # Affichage des données
  output$dataTable <- renderDT({
    req(dataProcessed())
    dataProcessed()
  }, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  
  # Générer le tableau des valeurs manquantes uniquement lorsque l'utilisateur clique sur "Détails" sous "Variables manquantes"
  output$missingDetailsTable <- renderDT({
    if(currentView() == "missing") {
      req(dataProcessed())
      calculateMissingDetails(dataProcessed())
    }
  }, options = list(pageLength = 5, searching = FALSE, scrollX = TRUE))
  
  
  
  # Générer le tableau des outliers uniquement lorsque l'utilisateur clique sur "Détails" sous "Outliers"
  output$outliersTable <- renderDT({
    if(currentView() == "outliers") {
      req(dataProcessed())
      detectOutliers(dataProcessed())
    }
  }, options = list(pageLength = 5, searching = FALSE, scrollX = TRUE))
  
  
  
  
  
  
  # Function to detect constant columns
  detectConstantColumns <- function(df) {
    constant_df <- sapply(df, function(x) length(unique(x)) == 1)
    const_cols <- names(df)[constant_df]
    
    data.frame(
      Variable = const_cols,
      stringsAsFactors = FALSE
    )
  }
  
  removeConstantColumns <- function(df, const_cols) {
    df[ , !(names(df) %in% const_cols)]
  }
  
  
  observeEvent(input$show_constant, {
    req(dataProcessed())
    constant_cols_df <- detectConstantColumns(dataProcessed())
    reactiveConstantColumns(constant_cols_df) # Store the constant columns
    
    
    output$dynamicTableUI <- renderUI({
      if (nrow(reactiveConstantColumns()) > 0) {
        tagList(
          DTOutput("constantColsTable"),
          hr(),
          actionButton("remove_const_cols", "Remove Constant Columns")
        )
      } else {
        tagList(
          h4("No constant columns detected.")
        )
      }
    })
  })
  
  
  output$constantColsTable <- renderDT({
    req(reactiveConstantColumns())
    reactiveConstantColumns()
  }, options = list(pageLength = 5, searching = FALSE, scrollX = TRUE))
  
  
  observeEvent(input$remove_const_cols, {
    req(reactiveConstantColumns())
    data <- dataProcessed() # Get the current data
    const_cols <- reactiveConstantColumns()$Variable
    
    updated_data <- removeConstantColumns(data, const_cols)
    dataProcessed(updated_data) # Update the processed data without constant columns
    output$dynamicTableUI <- renderUI({
      h4("Constant columns removed.")
    })
  })
  
  
  # Mettre à jour le tableau des détails des valeurs manquantes dynamiquement
  # Observateur pour afficher les détails des valeurs manquantes
  observeEvent(input$show_missing, {
    req(dataProcessed())
    df_details <- calculateMissingDetails(dataProcessed())
    
    
    # Définir le contenu UI pour le tableau et les options de traitement
    output$dynamicTableUI <- renderUI({
      tagList(
        DTOutput("missingDetailsTable"), # Affiche d'abord le tableau
        hr(), # Ajouter une séparation visuelle
        selectInput("quantitative_method", "Méthode pour les variables quantitatives:", 
                    choices = c("Moyenne" = "mean", "Médiane" = "median", "Mode" = "mode")),
        selectInput("qualitative_method", "Méthode pour les variables qualitatives:", 
                    choices = c("Mode" = "mode", "Nouvelle Catégorie" = "new_category")),
        actionButton("apply_mv_treatment", "Appliquer")
      )
    })
    
    # Configurer l'affichage du tableau des valeurs manquantes
    output$missingDetailsTable <- renderDT({
      df_details
    }, options = list(pageLength = 5, searching = FALSE, scrollX = TRUE))
  })
  
  
  # Appliquer les méthodes d'imputation lorsque demandé
  observeEvent(input$apply_mv_treatment, {
    req(dataProcessed(), input$quantitative_method, input$qualitative_method)
    df <- imputeData(dataProcessed(), input$quantitative_method, input$qualitative_method)
    dataProcessed(df) # Mettre à jour les données traitées
    # Si vous souhaitez mettre à jour le tableau des détails des valeurs manquantes après l'imputation
    output$missingDetailsTable <- renderDT(calculateMissingDetails(df), options = list(pageLength = 5, searching = FALSE))
  })
  
  
  
  
  
  # Pour stocker les noms des variables catégorielles et numériques
  varTypes <- reactiveVal(list(categorical = character(), numerical = character()))
  
  # Lorsque les données sont chargées ou mises à jour
  observeEvent(dataProcessed(), {
    df <- dataProcessed()
    # Détecter les types de variables
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    # Stocker les types de variables dans une réactive value
    varTypes(list(categorical = cat_vars, numerical = num_vars))
  })
  
  # Générer le tableau pour l'affichage des types de variables
  output$tableVariableTypes <- renderDT({
    var_types <- varTypes()
    # Trouver la longueur maximale
    max_length <- max(length(var_types$categorical), length(var_types$numerical))
    
    # Étendre les deux listes à la longueur maximale
    categorical_vars <- c(var_types$categorical, rep(NA, max_length - length(var_types$categorical)))
    numerical_vars <- c(var_types$numerical, rep(NA, max_length - length(var_types$numerical)))
    
    # Créer un data frame pour l'affichage
    data.frame(
      Categorical = categorical_vars,
      Numerical = numerical_vars,
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 5, searching = TRUE, scrollX = TRUE))
  
  
  
  # Action to remove outliers
  observeEvent(input$apply_outlier_action, {
    req(input$outlier_action == "Remove")  # Ensure "Remove" is selected
    df <- dataProcessed()                   # Get the current data
    #outliers <- reactiveOutliers()          # Get the current outliers
    outliers_df <- detectOutliers(df)  
    
    if (nrow(outliers_df) > 0) {
      # Loop through all variables and remove outliers
      for (var in unique(outliers_df$Variable)) {
        outlier_values <- outliers_df$OutlierValue[outliers_df$Variable == var]
        df <- df[!df[[var]] %in% outlier_values, ]
      }
      dataProcessed(df)  # Update the processed data without outliers
      reactiveOutliers(NULL)  # Reset the reactive value for outliers
    }
    
    # Update the table to reflect the changes
    output$outliersTable <- renderDT({
      detectOutliers(dataProcessed())
    }, options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE))
  })
  
  output$visualisationPlot <- renderPlot({
    req(dataOriginal, input$visualizationType,input$numObservations)
    
    # Identifier les variables numériques
    numeric_vars <- sapply(dataOriginal(), function(column) is.numeric(column))
    
    # Extraire les colonnes des variables numériques
    df_numeric <- dataOriginal()[numeric_vars]
    
    # Sélection des premières observations en fonction du slider
    df_subset <- df_numeric[1:input$numObservations, ]
    
    # Calcul de l'ACP
    acp_result <- PCA(df_subset, graph = FALSE)
    
    if (input$visualizationType == "var") {
      # Affichage du cercle des corrélations pour l'ACP sur les deux premières dimensions
      fviz_pca_var(acp_result, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, title = "ACP - Variables")
    } else if (input$visualizationType == "ind") {
      # Affichage du cercle des corrélations pour l'ACP sur les deux premières dimensions
      fviz_pca_ind(acp_result, col.ind = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, title = "ACP - Individus")
    }
  })
  
  trained_model_class <- reactiveValues( svm = NULL, rf = NULL)
  test_class <- reactiveValues(X_test_class = NULL, Y_test_class = NULL)
  train_class <- reactiveValues(dat = NULL, X_train_class = NULL, Y_train_class = NULL)
  probabilities <- reactiveValues(svm = NULL, rf = NULL)
  predictions <- reactiveValues(svm = NULL, rf = NULL)
  
  observeEvent(input$trainClassification, {
    req(dataOriginal(), input$file1, input$targetVariable, probabilities, predictions, train_class, test_class)
    
    df <- imputeData(dataOriginal(), input$quantitative_method, input$qualitative_method)
    #df <- dataOriginal()
    
    # Séparer les données en variables explicatives (X) et variable cible (Y)
    X <- df[, -which(names(df) == input$targetVariable, arr.ind = TRUE)]
    Y <- df[, input$targetVariable]
    Y_binary <- ifelse(Y == 2, 1, 0)
    
    # Diviser les données en ensembles d'entraînement et de test (80% train, 20% test)
    set.seed(123)  # Pour la reproductibilité
    splitIndex <- createDataPartition(Y_binary, p = 0.8, list = FALSE)
    train_class$X_train_class <- X[splitIndex, ]
    train_class$Y_train_class <- Y_binary[splitIndex]
    test_class$X_test_class <- X[-splitIndex, ]
    test_class$Y_test_class <- Y_binary[-splitIndex]
    
    train_class$dat <- cbind( train_class$X_train_class, y = as.factor( train_class$Y_train_class))
    
    # Entraîner le modèle SVM
    trained_model_class$svm <- svm(y ~ ., data = train_class$dat, kernel = "linear", scale = FALSE, probability=TRUE)
    
    # Entraîner le modèle Random Forest
    trained_model_class$rf <- randomForest(y ~ .,  data = train_class$dat,  importance = TRUE, proximity = TRUE)
    
    dat_test <- cbind(test_class$X_test_class, as.factor(test_class$Y_test_class))
    
    # Faire des prédictions avec le modèle SVM
    predictions$svm <- predict(trained_model_class$svm, newdata = dat_test)
    
    # Obtenir les probabilités postérieures pour la classe 1
    probabilities$svm <- attr(predict(trained_model_class$svm, newdata = dat_test, probability = TRUE), "probabilities")[, 2]
    
    # Faire des prédictions avec le modèle Random Forest
    predictions$rf <- predict(trained_model_class$rf, newdata = dat_test)
    
    # Use the probability of the positive class (class '1')
    probabilities$rf <- as.numeric(predict(trained_model_class$rf, newdata = dat_test, type = "prob")[, 2])
    
  })
  
  
  output$rocSvm <- renderPlot({
    req(test_class, probabilities)
    

    # Créer la courbe ROC avec les probabilités postérieures
    roc_svm <- roc(test_class$Y_test_class, probabilities$svm)
    
    # Tracer le graphique ROC
    plot(roc_svm, col = "blue", lwd = 2, main = "Courbe ROC - SVM")
    
    
    # Ajouter une légende avec la valeur AUC
    legend("bottomright", legend = paste("AUC =", round(auc(roc_svm), 3)), col = "white", bg = "transparent", cex = 0.8)
  })
  
  output$rocTree <- renderPlot({
    req(test_class, probabilities)
    
    roc_rf <- roc(test_class$Y_test_class, probabilities$rf)
    plot(roc_rf, col = "blue", lwd = 2, main = "Courbe ROC - RANDOM FOREST")
    
    # Ajouter une légende avec la valeur AUC
    legend("bottomright", legend = paste("AUC =", round(auc(roc_rf), 3)), col = "white", bg = "transparent", cex = 0.8)
  })
  
  output$confusionSVM <- renderPlotly({
    req(predictions,test_class)
    
    # Construire la matrice de confusion
    confusion_svm <- confusionMatrix(predictions$svm, as.factor(test_class$Y_test_class))
    
    # Afficher la matrice de confusion
    plot_confusion_matrix(confusion_svm, title = "Matrice de Confusion - SVM")
  })
  
  output$confusionTree <- renderPlotly({
    req(predictions,test_class)
    
    # Construire la matrice de confusion
    confusion_rf <- confusionMatrix(predictions$rf, as.factor(test_class$Y_test_class))
    
    # Afficher la matrice de confusion
    plot_confusion_matrix(confusion_rf, title = "Matrice de Confusion - RANDOM FOREST")
  })
  
  trained_model_reg <- reactiveValues( lm = NULL, rpart = NULL, rf = NULL, ridge = NULL)
  test_reg <- reactiveValues(X_test_reg = NULL, Y_test_reg = NULL)
  train_reg <- reactiveValues(dat_reg = NULL, X_train_reg = NULL, Y_train_reg = NULL)
  
  observeEvent(input$trainRegression, {
    req(dataOriginal(), input$targetVariablereg, train_reg, test_reg, trained_model_reg)
    
    # Identifier les variables numériques
    numeric_vars <- sapply(dataOriginal(), function(column) is.numeric(column))
    
    # Extraire les colonnes des variables numériques
    df_numeric <- dataOriginal()[numeric_vars]
    
    # Séparer les données en variables explicatives (X) et variable cible (Y)
    X <- df_numeric[, -which(names(df_numeric) == input$targetVariablereg, arr.ind = TRUE)]
    Y <- df_numeric[, input$targetVariablereg]
    
    # Diviser les données en ensembles d'entraînement et de test (80% train, 20% test)
    set.seed(123)  # Pour la reproductibilité
    splitIndex <- createDataPartition(Y, p = 0.8, list = FALSE)
    train_reg$X_train_reg <- X[splitIndex, ]
    train_reg$Y_train_reg <- Y[splitIndex]
    test_reg$X_test_reg <- X[-splitIndex, ]
    test_reg$Y_test_reg <- Y[-splitIndex]
    
    
    trained_model_reg$lm <- lm(train_reg$Y_train_reg ~ ., data = train_reg$X_train_reg )
    trained_model_reg$rpart <- rpart(train_reg$Y_train_reg ~ ., data = train_reg$X_train_reg , method = "anova")
    #trained_model_reg$rf <- randomForest(train_reg$X_train_reg , train_reg$Y_train_reg)
    #trained_model_reg$lr <- train(train_reg$X_train_reg , train_reg$Y_train_reg, method = "ridge")
    
  })
  
  output$reslm <- renderPlot({
    req(predictions,train_reg)
    
    # Récupérer les résidus du modèle de régression linéaire
    residuals <- residuals(trained_model_reg$lm)
    
    # Créer un graphique de résidus avec plot
    plot(train_reg$Y_train_reg, residuals, main = "Graphique de Résidus",
         xlab = "Valeurs Observées", ylab = "Résidus")
  })
  
  output$resrpart <- renderPlot({
    req(trained_model_reg,train_reg)
    
    # Récupérer les résidus du modèle de régression linéaire
    residuals <- residuals(trained_model_reg$rpart)
    
    # Créer un graphique de résidus avec plot
    plot(train_reg$Y_train_reg, residuals, main = "Graphique de Résidus",
         xlab = "Valeurs Observées", ylab = "Résidus")
  })
  
  
  output$metriquesreg <- renderPlot({
    req(test_reg, trained_model_reg)
    
    # Comparer différentes métriques entre les deux modèles (ajouté)
    metrics_lm <- calculate_metrics(trained_model_reg$lm, test_reg$X_test_reg, test_reg$Y_test_reg)
    metrics_rf <- calculate_metrics(trained_model_reg$rpart, test_reg$X_test_reg, test_reg$Y_test_reg)
    
    metrics_df <- data.frame(
      Model = c("Régression Linéaire", "Random Forest"),
      RMSE = c(metrics_lm["RMSE"], metrics_rf["RMSE"]),
      MAE = c(metrics_lm["MAE"], metrics_rf["MAE"])
    )
    
    # Créer un graphique à barres
    barplot(t(as.matrix(metrics_df[, -1])), beside = TRUE, col = c("blue", "green"), 
            legend.text = metrics_df$Model, 
            main = "Comparaison des Métriques entre les Modèles",
            ylab = "Scores", xlab = "Métriques")
  })
  
  
}

shinyApp(ui, server)