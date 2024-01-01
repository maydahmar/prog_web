library(shiny)
library(shinydashboard)

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

# Fonction d'analyse des variables
analyseVariables <- function(df) {
  sapply(df, function(column) {
    type <- ifelse(is.numeric(column) || is.integer(column), "Quantitative", "Qualitative")
    categories <- ifelse(type == "Qualitative", length(unique(column)), NA)
    
    # Gestion des valeurs manquantes pour les calculs de quantile et d'IQR
    if (type == "Quantitative") {
      columnClean <- na.omit(column)
      outliers <- sum(columnClean < quantile(columnClean, 0.25) - 1.5 * IQR(columnClean) | 
                        columnClean > quantile(columnClean, 0.75) + 1.5 * IQR(columnClean))
    } else {
      outliers <- NA
    }
    
    missingValues <- sum(is.na(column))
    c(Type = type, Categories = categories, Outliers = outliers, MissingValues = missingValues)
  })
}

# Fonction d'analyse du déséquilibre des classes
analyseDeséquilibreClasses <- function(df, targetVariable) {
  if (is.factor(df[[targetVariable]]) || is.character(df[[targetVariable]])) {
    table(df[[targetVariable]])
  } else {
    NULL
  }
}

#Fonction pour calculer le mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analyse Avancée de Données"),
  dashboardSidebar(
    fileInput("file1", "Choisir un fichier", accept = c(".csv", ".dat", ".txt")),
    actionButton("load", "Charger les données"),
    checkboxInput("normalize", "Normaliser les données", value = FALSE),
    checkboxInput("dummy", "Dummifier les données", value = FALSE),
    selectInput("targetVariable", "Variable Cible", choices = NULL)
  ),
  dashboardBody(
    tabsetPanel(id = "mainTabset",
                tabPanel("Données",
                         fluidRow(
                           uiOutput("dataSummaryUI")
                         ),
                         fluidRow(
                           uiOutput("missingDataOptionsUI")
                         )
                ),
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Déséquilibre des Classes", plotOutput("classImbalance"))
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values for data and missing data info
  values <- reactiveValues(data = NULL, missingInfo = NULL)
  
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
  

}

shinyApp(ui, server)
