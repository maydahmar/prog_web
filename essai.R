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
library(shinydashboard)
library(corrplot)
library(ggplot2)
library(philentropy)


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

# Univar --------------------------
#Fonction de camembert  
pie <- function(dataset, x.name) {
  fig <- plot_ly(dataset, labels = ~dataset[[x.name]], type = 'pie')
  fig <- fig %>% layout(
    title = paste0("Proportion de ", x.name),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )
  return(fig)
}

# Fonction pour créer un diagramme en bâtons des effectifs 
createBarPlot <- function(data, variable, x_label, y_label, title) {
  freq_table <- table(data[[variable]])
  fig <- plot_ly(x = as.numeric(names(freq_table)), y = freq_table, type = 'bar')
  fig <- fig %>% layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = title)
  return(fig)
}

button.to.remove <- c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian")

# Fonction pour créer un diagramme cumulatif des effectifs 
createCumulativePlot <- function(x, x_label, y_label, title) {
  freq_table <- table(x)
  freq_x <- freq_table/sum(length(x))
  cum_freq <- cumsum(freq_x)
  fig <- plot_ly(x = names(freq_table), y = cum_freq, type = 'bar')
  fig <- fig %>% layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = title)
  return(fig)
}

# Fonction pour créer un boite a moustache 
createBoxplot <- function(data, variable, y_label, title, box_size = 0.3) {
  fig <- plot_ly(data, y = ~data[[variable]], type = 'box', boxpoints = 'outliers')
  fig <- fig %>% layout(yaxis = list(title = y_label), title = title)
  return(fig)
}

# Fonction pour créer un courbe cululitive 
createCumulativePlotCont <- function(data, variable, y_label, title) {
  freq_table <- table(data[[variable]])
  cum_freq <- cumsum(freq_table)
  x_vals <- as.numeric(names(freq_table))
  y_vals <- cum_freq
  fig <- plot_ly() %>%
    add_trace(
      type = 'scatter',
      mode = 'markers+lines',
      x = x_vals,
      y = y_vals,
      marker = list(color = 'green4'),
      line = list(color = 'green4', width = 2)
    ) %>%
    layout(xaxis = list(title = variable), yaxis = list(title = y_label), title = title)
  return(fig)
}

#Fonction pour cree un histogramme pour variable continue 
createHistogram <- function(data, variable, bins, x_label, y_label, title) {
  fig <- plot_ly()
  
  if (!is.null(bins)) {
    fig <- fig %>% add_histogram(x = ~data[[variable]], histnorm = 'probability density', nbinsx = bins)
  } else {
    fig <- fig %>% add_histogram(x = ~data[[variable]], histnorm = 'probability density')
  }
  
  fig <- fig %>% layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = title)
  return(fig)
}

#Fonction pour cree un courbe cumulative pour variable continue 
createCumulativePlot <- function(data, variable, bins, x_label, y_label, title) {
  fig <- plot_ly()
  
  if (!is.null(bins)) {
    # Créer l'histogramme avec le nombre de bacs spécifié
    hist <- hist(data[[variable]], breaks = bins, plot = FALSE)
    # Calculer les fréquences cumulées
    cum_freq <- cumsum(hist$counts)
    # Tracer la courbe cumulative
    fig <- fig %>% add_trace(
      type = 'scatter',
      mode = 'lines',
      x = hist$mids,
      y = cum_freq,
      line = list(color = 'blue'),
      name = 'Cumulative'
    )
  } else {
    # Créer l'histogramme avec le nombre de bacs par défaut
    fig <- fig %>% add_histogram(
      x = ~data[[variable]],
      histnorm = 'probability density'
    )
    # Calculer les fréquences cumulées à partir de l'histogramme
    cum_freq <- cumsum(fig$data[[1]]$y)
    # Tracer la courbe cumulative
    fig <- fig %>% add_trace(
      type = 'scatter',
      mode = 'lines',
      x = fig$data[[1]]$x,
      y = cum_freq,
      line = list(color = 'blue'),
      name = 'Cumulative'
    )
  }
  # Configurer la mise en page
  fig <- fig %>% layout(
    xaxis = list(title = x_label),
    yaxis = list(title = y_label),
    title = title
  )
  
  return(fig)
}


#Bivar---------

#Fonction pour cree un nuage de points pour deux variables 
createScatterPlot <- function(data, x_variable, y_variable, x_label, y_label, title) {
  fig <- plot_ly(data, x = ~data[[x_variable]], y = ~data[[y_variable]], mode = 'markers')
  
  fig <- fig %>% layout(
    xaxis = list(title = x_label),
    yaxis = list(title = y_label),
    title = title
  )
  
  return(fig)
}



#Fonction pour cree un nuage de points pour deux variables 
calculate_and_visualize_correlation <- function(dataset, var1, var2) {
  # Subset the dataset to include only the relevant columns
  subset_data <- dataset[, c(var1, var2), drop = FALSE]
  # Remove rows with missing values in either variable
  subset_data <- na.omit(subset_data)
  # Calculate the correlation matrix
  correlation_matrix <- cor(subset_data)
  # Visualize the correlation matrix
  corrplot(correlation_matrix, method = "color")
}

# Function to calculate and visualize the correlation
calculate_and_plot_correlation <- function(data) {
  # Exclure les colonnes avec une variance nulle
  non_constant_vars <- sapply(data, function(x) {
    var_value <- var(x, na.rm = TRUE)
    if (var_value == 0) {
      cat("Variance nulle dans la colonne:", names(x), "\n")
    }
    var_value > 0
  })
  
  data <- data[, non_constant_vars, drop = FALSE]
  
  # Filtrer uniquement les variables numériques
  data_num <- Filter(is.numeric, data)
  
  # Vérifier les statistiques descriptives pour les colonnes problématiques
  problematic_columns <- names(data_num)[sapply(data_num, function(x) sd(x, na.rm = TRUE) == 0)]
  if (length(problematic_columns) > 0) {
    cat("Colonnes avec écart-type zéro:", problematic_columns, "\n")
  }
  
  # Calculer la matrice de corrélation
  correlation_matrix <- cor(data_num, use = "complete.obs")
  
  # Créer un graphique de corrélation
  corrplot(correlation_matrix, method = "color")
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
                tabPanel(" Visualisation", value = "visual_panel",
                         fluidRow(
                           box(title = "Uni var", status = "primary", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_Uni_var", "Détails")),
                           box(title = "Bi Var", status = "warning", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_Bi_var", "Détails")),
                         ),
                         uiOutput("visualUI"))
                ,
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
    
    df <- dataOriginal()
    
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

  # Visualisation 

  # Utilisez une valeur réactive pour contrôler quel tableau est affiché
  currentViewVisual <- reactiveVal(NULL)
  observeEvent(input$show_Uni_var, {
    currentViewVisual("uni_var")
  })
  
  # Observateur pour le bouton d'affichage des données
  observeEvent(input$show_Bi_var, {
    # Utiliser la valeur réactive pour afficher les données
    currentViewVisual("bi_var")
  })
  
  output$visualUI <- renderUI({
    if(currentViewVisual() == "uni_var") {
    fluidPage(
      fluidRow(
        h2("Variable Categorielle"),
        fluidRow(
          box( width = 15,selectInput("univarCategorical", "choisir la variable", choices = NULL)
          )),
        fluidRow(
          box(title = "Diagrammes en secteurs", height = 460, width = 15,
              plotlyOutput("univarVariablePlot") )
          ),
        h2("Variable Numerique"),
        fluidRow(
          h3("Variable Numerique Discrete"),
          box( width = 15,selectInput("univarNumericaldisc", "choisir la variable", choices = NULL)
          )),
        fluidRow(
          box(height = 460, width = 6,plotlyOutput("univarNumericalbarplot") ),
          box(height = 460, width = 6,plotlyOutput("univarNumericalbarcumul"))
          ),
        fluidRow(
          box(height = 460, width = 12,plotlyOutput("univarNumericalbarBmoust"))
        ),
        fluidRow(h3("Variable Numerique Continue"),
          box( width = 15,selectInput("univarNumericalcont", "choisir la variable", choices = NULL),
           sliderInput("bins", "Nombre de classes (K):", min = 1, max = 50, value = 10)
          )),
        fluidRow(
          box(height = 460, width = 6,plotlyOutput("univarNumericalHist") 
          ),
          box(height = 460, width = 6, plotlyOutput("univarNumericalCden")
              )
          ),
      )
      )
      # Générer la page  de bi var  uniquement lorsque l'utilisateur clique sur "Détails" 
    } else if(currentViewVisual() == "bi_var") {
      fluidPage(
        h1("tous les var Numeriques"),
        fluidRow(
          box(title="Correlation Matrix", height = "460", width = 15,
              plotOutput("correlation") 
          )),
        h1("Interaction entre deux  variable"),
        
        fluidRow(height = "460", width = 15,
                 box(title = "Numerique vs Categorielle", width = 15,
                     selectInput("numericalVScategorical", "Numerique", choices = NULL),
                     selectInput("categoricalVSnumerical", "Categorielle", choices = NULL)
                 )),
        fluidRow(         
          box(height = "460", width = 15,
              plotlyOutput("boite_para")
          )),
        fluidRow(height = "460", width = 15,
                 box(title = "Numerique vs Numerique", width = 15,
                     selectInput("numericalVSnumerical", "Numerique", ""),
                     selectInput("numericalVSnumerical2", "Numerique", "")
                 )),
        fluidRow(
          box(height = "460", width = 15,
              plotlyOutput("scatter")
          )),
        fluidRow(height = "460", width = 15,
                box(title = "Categorielle vs Categorielle", width = 15,
                    selectInput("categoricalVScategorical", "Categorielle", choices = NULL),
                    selectInput("categoricalVScategorical2", "Categorielle", choices = NULL)
          )),
        fluidRow(
          box(width = 15, height = "460",
              plotlyOutput("histo_profil")
          )),
        fluidRow(
          h3("Les métriques d'evaluation "),
          box(width = 15, height = 250,
              tableOutput("association_table")
          ))
      
      ) 
      }
    })
  
  # Définition de la fonction pour analyser les variables
  analyse_variables <- function(data) {
    numeric <- names(Filter(function(x) is.numeric(x) , data))
    numeric_discrete <- names(Filter(function(x) is.numeric(x) && length(unique(x)) <= 10, data))
    numeric_continuous <- names(Filter(function(x) is.numeric(x) && length(unique(x)) > 10, data))
    categorical <- names(Filter(function(x) is.factor(x) || is.character(x), data))
    
    return(list(numeric_discrete = numeric_discrete,
                numeric_continuous = numeric_continuous,
                categorical = categorical,
                numeric= numeric))
  }
  
  variables <- reactive({
    analyse_variables(dataOriginal())
  })
  
  observe({
    req(dataOriginal(),variables())
    
    updateSelectInput(session, "numericalVScategorical", choices = variables()$numeric ,selected = input$numericalVScategorical)
    
    updateSelectInput(session, "categoricalVSnumerical", choices = variables()$categorical  ,selected = input$categoricalVSnumerical)
    
    updateSelectInput(session, "categoricalVScategorical", choices = variables()$categorical ,selected = input$categoricalVScategorical)
    
    updateSelectInput(session, "categoricalVScategorical2", choices = variables()$categorical  ,selected = input$categoricalVScategorical2)
  
    updateSelectInput(session, "numericalVSnumerical", choices = variables()$numeric,selected = input$numericalVSnumerical)
   
    updateSelectInput(session, "numericalVSnumerical2", choices = variables()$numeric,selected = input$numericalVSnumerical2)
  
    updateSelectInput(session, "univarCategorical", choices = variables()$categorical  ,selected = input$univarCategorical )
    
    updateSelectInput(session, "univarNumericaldisc", choices = variables()$numeric_discrete ,selected = input$univarNumericaldisc)
    
    updateSelectInput(session, "univarNumericalcont", choices = variables()$numeric_continuous  ,selected = input$univarNumericalcont)
  })
  
  #------------------Uni variable------------------------------  
  ## var categorielle-------   
  # Fonction pour créer le diagramme circulaire
  output$univarVariablePlot <- renderPlotly({
    req(dataOriginal(),input$univarCategorical)
    df <- dataOriginal()
    pie_univar <- plot_ly(
      labels = ~df[[input$univarCategorical]],
      type = 'pie',
      marker = list(colors = 'Set1')
    )
    pie_univar
  })
  
  ## var num discert ---------------  
  
  # Commande pour l'affichage du plot des effectifs
  output$univarNumericalbarplot <- renderPlotly({
    req(input$univarNumericaldisc)  # Assurez-vous que la variable est sélectionnée
    createBarPlot(dataOriginal(), input$univarNumericaldisc, input$univarNumericaldisc, "Fréquences", "Fréquences ")
  })
  
  # Commande pour l'affichage du plot des fréquences cumulées
  output$univarNumericalbarcumul <- renderPlotly({
    req(input$univarNumericaldisc)  # Assurez-vous que la variable est sélectionnée
    createCumulativePlotCont(dataOriginal(),input$univarNumericaldisc, "Fréquences cumulées", "Fréquences cumulées ")
  })
  
  # Commande pour l'affichage du plot de Boîte à moustaches
  output$univarNumericalbarBmoust <- renderPlotly({
    req(input$univarNumericaldisc)  # Assurez-vous que la variable est sélectionnée
    createBoxplot(dataOriginal(), input$univarNumericaldisc, input$univarNumericaldisc, "Boîte à moustaches ", 0.3)
  })

  ## var num cont-------------------
  # Affichage  de l'histogramme
  output$univarNumericalHist <- renderPlotly({
    req(input$univarNumericalcont)  # Assurez-vous que la variable est sélectionnée
    createHistogram(dataOriginal(), input$univarNumericalcont, bins = input$bins, 
                    x_label = input$univarNumericalcont, y_label = "Fréquence", 
                    title = paste("Histogramme de", input$univarNumericalcont))
  })

  # Affichage de la courbe cumulative 
  output$univarNumericalCden <- renderPlotly({
    req(input$univarNumericalcont, input$bins)  # Assurez-vous que la variable et le nombre de bacs sont sélectionnés
    
    # Appeler la fonction createCumulativePlot
    createCumulativePlot(
      data = dataOriginal(),
      variable = input$univarNumericalcont,
      bins = input$bins,
      x_label = input$univarNumericalcont,
      y_label = "Fréquences cumulées",
      title = paste("Courbe cumulative de", input$univarNumericalcont)
    )
  })
  
  #------------------bi variable------------------------------ 
  ## all var num----------
  # Filtrer les variables avec une variance non nulle
  non_constant_vars <- reactive({
    df <- dataOriginal()
    # Sélectionner uniquement les colonnes numériques
    # df[sapply(df, is.numeric)]
    Filter(function(x) is.numeric(x) && sd(x)!= 0 , df)
  })
  # Calculer la matrice de corrélation lorsque les données sont chargées
  correlation_matrix <- reactive({
    cor(non_constant_vars(), use = "pairwise.complete.obs")
  })
  # Afficher la matrice de corrélation en utilisant corrplot
  output$correlation <- renderPlot({
    corrplot(correlation_matrix(), method = "color")
  })
  

 
  
  
  ## num/num nuage de points -------------
  output$scatter <- renderPlotly({
    req(dataOriginal(),input$numericalVSnumerical,input$numericalVSnumerical2)
    data <- dataOriginal()
    # Sélection des variables en fonction des sélecteurs d'entrée
    x_variable <- data[[input$numericalVSnumerical]]
    y_variable <- data[[input$numericalVSnumerical2]]
    
    # Création du graphique scatter avec plot_ly
    plot_ly(data, x = x_variable, y = y_variable, mode = "markers") %>%
      layout(title = "Scatter Plot",
             xaxis = list(title = input$numericalVSnumerical),
             yaxis = list(title = input$numericalVSnumerical2))
  })
  
  ## num/ cat boite parallele--------
  output$boite_para <- renderPlotly({
    req(dataOriginal(), input$numericalVScategorical, input$categoricalVSnumerical)
    data <- dataOriginal()
    
    plot_ly(data, x = ~get(input$categoricalVSnumerical), y = ~get(input$numericalVScategorical), type = "box", color = ~get(input$categoricalVSnumerical)) %>%
      layout(title = paste("Boîtes à moustaches de", input$numericalVScategorical, "pour chaque catégorie", input$categoricalVSnumerical),
             xaxis = list(title = input$categoricalVSnumerical),
             yaxis = list(title = input$numericalVScategorical))
  })
  
   ## cat/cat diag profil
  
  ## Utilisez les noms de colonnes dynamiques--------
    data_aggregated <- reactive({
      req(dataOriginal(), input$categoricalVScategorical, input$categoricalVScategorical2)
      
      df <- dataOriginal()
      col1 <- input$categoricalVScategorical
      col2 <- input$categoricalVScategorical2
      
      data_aggregated <- df %>%
        group_by(!!sym(col1), !!sym(col2)) %>%
        summarise(Frequency = n()) %>%
        ungroup()
      
      return(data_aggregated)
    })

  output$histo_profil <- renderPlotly({
    req(data_aggregated())
    # Utilisez les noms de colonnes dynamiques
    col1 <- input$categoricalVScategorical
    col2 <- input$categoricalVScategorical2
    
    gg <- ggplot(data_aggregated(), aes(x = !!sym(col1), y = Frequency, fill = !!sym(col2))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      labs(x = col1, y = "Fréquence", fill = col2) +
      theme_minimal()
    
    # Convertissez le ggplot en plotly
    ggplotly(gg)
  })
  
  
  
  ## mesures---------
  output$association_table <- renderTable({
    req(dataOriginal(), input$categoricalVScategorical, input$categoricalVScategorical2)
    # Sélection des variables
    data <- dataOriginal()
    var1 <- data[[input$categoricalVScategorical]]
    var2 <- data[[input$categoricalVScategorical2]]
    
    # Tableau de contingence
    contingency_table <- table(var1, var2)
    
    # Calcul des mesures d'association
    chi2 <- chisq.test(contingency_table, simulate.p.value = TRUE)$statistic
    phi2 <- sqrt(chi2 / (sum(contingency_table) * min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)))
    cramer_v <- sqrt(phi2 / min(nrow(contingency_table) - 1, ncol(contingency_table) - 1))
    tschuprow_t <- sqrt(chi2 / sum(contingency_table))
    
    # Création d'un tableau avec les mesures
    result_table <- data.frame(
      Mesure = c("Khi-2", "Phi2", "Cramer's V", "Tschuprow's T"),
      Valeur = c(chi2, phi2, cramer_v, tschuprow_t)
    )
    result_table
  })
  
  
  
  #------------------ desequilibre------------------------------ 
}
shinyApp(ui, server)