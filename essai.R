library(shiny)
library(ggplot2)

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
    # Logic to calculate missing data info goes here
    # For example, count the number of NA values for each variable
    values$missingInfo <- sapply(values$data, function(x) sum(is.na(x)))
    
    # Create a UI output for missing data
    tagList(
      box(title = "Nombre de valeurs manquantes", width = 3, status = "primary", solidHeader = TRUE,
          verbatimTextOutput("missingDataText"))
      # Add more boxes for other summaries as needed
    )
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
  
  output$missingDataText <- renderText({
    req(values$missingInfo)
    paste("Valeurs manquantes par variable:\n", toString(values$missingInfo))
  })
  
  observeEvent(input$applyTreatment, {
    req(values$data)
    treatment <- input$missingDataTreatment
    if (treatment == "mean") {
      # Replace missing values with the mean
      for (varName in names(values$missingInfo)) {
        if (values$missingInfo[varName] > 0) {
          values$data[[varName]][is.na(values$data[[varName]])] <- mean(values$data[[varName]], na.rm = TRUE)
        }
      }
    } else if (treatment == "omit") {
      # Remove rows with missing values
      values$data <- na.omit(values$data)
    }
    
    # Update the missingInfo after treatment
    values$missingInfo <- sapply(values$data, function(x) sum(is.na(x)))
  })
  
  # Output for plot and class imbalance goes here
  # ...
  
}

shinyApp(ui, server)