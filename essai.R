library(shiny)
library(shinydashboard)
library(DT)

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

# Fonction de dummification
dummifyData <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      model.matrix(~ x - 1)[, -1]
    } else {
      x
    }
  })
  return(df)
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
                           
                           box(title = "Variables manquantes", status = "primary", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_missing", "Détails"))
                           
                           
                         ),
                         uiOutput("dynamicTableUI") 
                ),
                tabPanel("Plot", value = "plot_panel", plotOutput("dataPlot")),
                tabPanel("Déséquilibre des Classes",
                         DTOutput("tableVariableTypes")
                )
    )
  )
)
# Server
server <- function(input, output, session) {
  dataOriginal <- reactiveVal(NULL)
  dataProcessed <- reactiveVal(NULL)
  reactiveOutliers <- reactiveVal(NULL)
  
  observeEvent(input$load, {
    inFile <- input$file1
    if (is.null(inFile)) {return(NULL)}
    
    ext <- tools::file_ext(inFile$datapath)
    df <- switch(ext,
                 csv = { read.csv(inFile$datapath) },
                 dat = { read.table(inFile$datapath) },
                 txt = { read.delim(inFile$datapath) },
                 data = read.table(inFile$datapath, header = TRUE, sep = ","),
                 stop("Type de fichier non supporté")
    )
    
    dataOriginal(df)
    dataProcessed(df)
  })
  # Appliquer ou annuler la normalisation et la dummification
  observe({
    req(dataOriginal())
    df <- dataOriginal()
    
    if (input$normalize) {
      df <- normalizeData(df)
    }
    
    if (input$dummy) {
      df <- dummifyData(df)
    }
    
    
    
    
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
  }, options = list(pageLength = 10, autoWidth = TRUE))
  
  # Générer le tableau des valeurs manquantes uniquement lorsque l'utilisateur clique sur "Détails" sous "Variables manquantes"
  output$missingDetailsTable <- renderDT({
    if(currentView() == "missing") {
      req(dataProcessed())
      calculateMissingDetails(dataProcessed())
    }
  }, options = list(pageLength = 5, searching = FALSE))
  
  
  
  # Générer le tableau des outliers uniquement lorsque l'utilisateur clique sur "Détails" sous "Outliers"
  output$outliersTable <- renderDT({
    if(currentView() == "outliers") {
      req(dataProcessed())
      detectOutliers(dataProcessed())
    }
  }, options = list(pageLength = 5, searching = FALSE))
  
  
  
  
  
  
  
  
  
  
  
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
    }, options = list(pageLength = 5, searching = FALSE))
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
  }, options = list(pageLength = 5, searching = TRUE))
  
  
  
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
    }, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  
  
  
}

shinyApp(ui, server)