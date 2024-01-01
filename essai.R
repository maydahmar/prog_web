library(shiny)
library(shinydashboard)
library(DT)

# Fonction pour calculer le mode
Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Fonction de normalisation
normalizeData <- function(column) {
  (column - min(column)) / (max(column) - min(column))
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
  na_detail <- data.frame(
    Variable = names(na_count),
    MissingCount = na_count,
    MissingPercent = (na_count / nrow(df)) * 100,
    Type = sapply(df, function(x) {
      if (is.numeric(x)) "Quantitative" else "Qualitative"
    })
  )
  na_detail
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analyse Avancée de Données"),
  dashboardSidebar(
    fileInput("file1", "Choisir un fichier", accept = c(".csv", ".dat", ".txt")),
    actionButton("load", "Charger les données"),
    checkboxInput("normalize", "Normaliser les données"),
    checkboxInput("dummy", "Dummifier les données")
  ),
  dashboardBody(
    tabsetPanel(id = "tabs",
                tabPanel("Données", value = "data_panel",
                         fluidRow(
                           box(title = "Variables manquantes", status = "primary", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_missing", "Détails")),
                           box(title = "Variables qualitatives", status = "warning", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_qualitative", "Détails")),
                           box(title = "Outliers", status = "danger", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_outliers", "Détails"))
                         ),
                         DTOutput("missingDetailsTable"),
                         uiOutput("data_imputation_ui")
                ),
                tabPanel("Plot", value = "plot_panel", plotOutput("dataPlot")),
                tabPanel("Déséquilibre des Classes", value = "imbalance_panel", plotOutput("classImbalancePlot"))
    )
  )
)

# Server
server <- function(input, output, session) {
  reactiveData <- reactiveVal(NULL)
  reactiveMissingDetails <- reactiveVal(NULL)
  
  observeEvent(input$load, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    if (input$normalize) df <- as.data.frame(lapply(df, normalizeData))
    if (input$dummy) df <- dummifyData(df)
    reactiveData(df)
    reactiveMissingDetails(calculateMissingDetails(df))
  })
  
  output$missingDetailsTable <- renderDT({
    req(reactiveMissingDetails())
    reactiveMissingDetails()
  }, options = list(pageLength = 5, searching = FALSE))
  
  output$data_imputation_ui <- renderUI({
    tagList(
      selectInput("quantitative_method", "Méthode pour les variables quantitatives:", 
                  choices = c("Moyenne" = "mean", "Médiane" = "median", "Mode" = "mode")),
      selectInput("qualitative_method", "Méthode pour les variables qualitatives:", 
                  choices = c("Mode" = "mode", "Nouvelle Catégorie" = "new_category")),
      actionButton("apply_mv_treatment", "Appliquer")
    )
  })
  
  observeEvent(input$apply_mv_treatment, {
    req(reactiveData(), input$quantitative_method, input$qualitative_method)
    df <- reactiveData()
    df <- imputeData(df, input$quantitative_method, input$qualitative_method)
    reactiveData(df) # Mettre à jour les données traitées
    reactiveMissingDetails(calculateMissingDetails(df))
  })
  
  # Outputs pour les plots et le déséquilibre des classes
  # ...
}

shinyApp(ui, server)
