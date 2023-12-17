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
ui <- fluidPage(
  titlePanel("Analyse Avancée de Données"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choisir un fichier", accept = c(".csv", ".dat", ".txt")),
      actionButton("load", "Charger"),
      checkboxInput("normalize", "Normaliser les données quantitatives", value = FALSE),
      checkboxInput("dummy", "Dummification pour variables qualitatives", value = FALSE),
      selectInput("variable", "Choisir une Variable pour le Graphique", choices = NULL),
      selectInput("targetVariable", "Variable Cible pour le Déséquilibre des Classes", choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Résumé", verbatimTextOutput("summary")),
        tabPanel("Graphique", plotOutput("plot")),
        tabPanel("Tableau", tableOutput("table")),
        tabPanel("Déséquilibre des Classes", plotOutput("classBalance"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {  # Ajoutez 'session' ici
  data <- reactiveVal(NULL)
  variableInfo <- reactiveVal(NULL)
  
  observeEvent(input$load, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    ext <- tools::file_ext(inFile$datapath)
    df <- switch(ext,
                 csv = { read.csv(inFile$datapath) },
                 dat = { read.table(inFile$datapath, header = TRUE) },
                 txt = { read.delim(inFile$datapath) },
                 stop("Type de fichier non supporté")
    )
    
    if(input$dummy) {
      df <- dummifyData(df)
    }
    
    if(input$normalize) {
      df <- as.data.frame(lapply(df, function(column) {
        if(is.numeric(column)) {
          normalizeData(column)
        } else {
          column
        }
      }))
    }
    
    variableInfo(analyseVariables(df))
    data(df)
    updateSelectInput(session, "variable", choices = names(df))
    updateSelectInput(session, "targetVariable", choices = names(df))
  })
  
  output$summary <- renderPrint({
    if (is.null(data())) return()
    cat("Résumé des données :\n")
    print(summary(data()))
    cat("\nInformations sur les Variables :\n")
    print(variableInfo())
  })
  
  output$plot <- renderPlot({
    if (is.null(data()) || is.null(input$variable)) return()
    ggplot(data(), aes_string(x = input$variable)) + 
      geom_bar() + 
      theme_minimal()
  })
  
  output$table <- renderTable({
    if (is.null(data())) return()
    head(data())
  })
  
  output$classBalance <- renderPlot({
    if (is.null(data()) || is.null(input$targetVariable)) return()
    classDistribution <- analyseDeséquilibreClasses(data(), input$targetVariable)
    if (!is.null(classDistribution)) {
      barplot(classDistribution, main = "Distribution des Classes", xlab = input$targetVariable, ylab = "Nombre d'observations")
    }
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
