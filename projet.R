library(shiny)

# Contenu de l'interface
ui <- fluidPage(
  # Titre de l'ui
  titlePanel("Variable qualitative"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        column(10, 
               # Bouton de recherche du fichier à charger
               fileInput(inputId = "file1", label = "Choose TSV File",
                         accept = c("text/plain", ".tsv"))
        )
      ),
      fluidRow(
        column(2, 
               # Buton de chargement 'en retard'
               actionButton(inputId = "go", label = "Load"))
      ),
      fluidRow(
        column(10,
               # Barre de défilement pour choisir le nombre d'observations
               sliderInput("num_observations", "Number of Observations", 1, 100, 10)
        )
      )
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Diagramme en colonnes", plotOutput(outputId = "colonnes")), 
        tabPanel("Diagramme en secteurs", plotOutput(outputId = "secteurs")),
        tabPanel("Table", tableOutput(outputId = "table"))
      )
    )
  )
)

# Commandes à exécuter
server <- function(input, output){
  
  # Recherche et chargement du fichier de données
  data <- eventReactive(input$go, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.table(inFile$datapath, header = TRUE, sep = "\t")
  })
  
 
  # Diagramme en colonnes initial
  output$colonnes <- renderPlot({
    barplot(table(data()[1:input$num_observations, ]), main = "Catégories Socioprofessionnelles", 
            ylab="Effectifs", las = 2,
            names.arg = substr(names(table(data()[1:input$num_observations, ])), 1, 4))
  })
  
  # Diagramme en secteurs
  output$secteurs <- renderPlot({
    pie(table(data()[1:input$num_observations, ]), labels = substr(names(table(data()[1:input$num_observations, ])), 1, 4), 
        main = "Catégories Socioprofessionnelles", col=c())
  })
  
  # Table des effectifs
  output$table <- renderTable({table(data()[1:input$num_observations, ])}, colnames = FALSE)
}
# Association interface & commandes
shinyApp(ui = ui, server = server)
