library(shiny)
library(shinydashboard)
library(DT)
library(plotly) 
library(tidyverse)

##-------------------------- Fonction  pretraitement --------------------------
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

##-------------------------- Fonction  visualisation --------------------------
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
createCumulativePlotCont <- function(x, x_label, y_label, title) {
  freq_table <- table(x)
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
    layout(xaxis = list(title = x_label), yaxis = list(title = y_label), title = title)
  return(fig)
}



# ------------------------------------------------------------ UI ------------------------------------------------------------
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
                           box(title = "Outliers", status = "danger", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_outliers", "Détails")),
                           box(title = "Variables qualitatives", status = "warning", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_qualitative", "Détails")),
                           
                           box(title = "Variables manquantes", status = "primary", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_missing", "Détails"))
                           
                           
                         ),
                         uiOutput("dynamicTableUI") 
                ),
                tabPanel("Plot", value = "plot_panel", plotOutput("dataPlot")),
                tabPanel("Déséquilibre des Classes", value = "imbalance_panel",
                         fluidRow(
                           box(title = "Uni var", status = "primary", solidHeader = TRUE, width = 4, 
                               collapsible = TRUE, actionButton("show_Uni_var", "Détails")),
                           box(title = "Bi Var", status = "warning", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_Bi_var", "Détails")),
                           box(title = "Splitting ", status = "danger", solidHeader = TRUE, width = 4,
                               collapsible = TRUE, actionButton("show_sample", "Détails")),
                           
                         ),
                         fluidRow(
                           box(title = "Variable", width = 15,
                               selectInput(
                                 "target", "Target Variable", choices = NULL
                               )) 
                         ),
                         uiOutput("visualUI")
                )
    )
  )
)


# ------------------------------------------------------------Server------------------------------------------------------------
server <- function(input, output, session) {
  dataOriginal <- reactiveVal(NULL)
  dataProcessed <- reactiveVal(NULL)
  
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
    
    dataOriginal(df)
    dataProcessed(df)
  })
  
  # Fonction pour filtrer les variables categorielle ou numerique 
  sortVariables <- function(data) {
    numeric_vars <- names(Filter(is.numeric, data))
    categorical_vars <- names(Filter(function(x) is.factor(x) | is.character(x), data))
    
    return(list(numerical = numeric_vars, categorical = categorical_vars))
  }
  
  sorted_vars <- sortVariables(df)
  
  # Fonction pour filtrer les variables numerique discret ou continue  
  sortQuantitativeVariables <- function(data) {
    quantitative_vars <- sorted_vars$numerical
    discrete_vars <- quantitative_vars[sapply(data[, quantitative_vars], function(x) length(unique(x)) < 10)] # Vous pouvez ajuster la limite 10 selon vos besoins
    continuous_vars <- setdiff(quantitative_vars, discrete_vars)
    
    return(list(discret = discrete_vars, continue = continuous_vars))
  }
  
  sorted_quant_vars <- sortQuantitativeVariables(df)
  
  
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
    
    updateSelectInput(session, "target", choices = names(df), selected = input$target)
    updateSelectInput(session, "numericalVScategorical", choices = #,selected = input$numericalVScategorical)
    )
    updateSelectInput(session, "categoricalVSnumerical", choices = #,selected = input$categoricalVSnumerical)
    )
    updateSelectInput(session, "numericalVSnumerical", choices = #,selected = input$numericalVSnumerical)
    )
    updateSelectInput(session, "numericalVSnumerical2", choices = #,selected = input$numericalVSnumerical2)
    )
    updateSelectInput(session, "univarCategorical", choices = sorted_vars$categorical  ,selected = input$univarCategorical )
    
    updateSelectInput(session, "univarNumericaldisc", choices = sorted_quant_vars$discret ,selected = input$univarNumericaldisc)
    
    updateSelectInput(session, "univarNumericalcont", choices = sorted_quant_vars$continue  ,selected = input$univarNumericalcont)
    
  })
  
  
  
  
  
  # Utilisez une valeur réactive pour contrôler quel tableau est affiché
  currentView <- reactiveVal(NULL)
  
  observeEvent(input$show_missing, {
    currentView("missing")
    
  })
  
  observeEvent(input$show_qualitative, {
    currentView("qualitative")
  })
  
  observeEvent(input$show_outliers, {
    currentView("outliers")
    
  })
  
  output$dynamicTableUI <- renderUI({
    if(currentView() == "missing") {
      DTOutput("missingDetailsTable")
    } else if(currentView() == "outliers") {
      DTOutput("outliersTable")
    }
  })
  
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
  
  
  
  # Utilisez une valeur réactive pour contrôler quel tableau est affiché
  currentBoxView <- reactiveVal(NULL)
  
  observeEvent(input$show_Uni_var, {
    currentBoxView("uni_var_details")
  })
  
  observeEvent(input$show_Bi_var, {
    currentBoxView("bi_var_details")
  })
  
  observeEvent(input$show_sample, {
    currentBoxView("sample_details")
  })
  
  
  
  
  # plot 
  output$visualUI <- renderUI({
    # Générer la page  de uni var  uniquement lorsque l'utilisateur clique sur "Détails" 
    if(currentBoxView() == "uni_var_details") {
      fluidPage(
        h2("Variable Categorielle"),
        fluidRow(
          box( width = 15,
               selectInput("univarCategorical", "choisir la variable", choices = NULL)
          )),
        fluidRow(
          box(title = "Diagrammes en secteurs", height = 460, width = 6,
              plotlyOutput("selectedUnivarCategorical")
          ),
          box(title = "Diagrammes en secteurs", height = 460, width = 6,
              plotlyOutput("univarTargetVariablePlot")
          )),
        h2("Variable Numerique"),
        fluidRow(
          h3("Variable Numerique Discrete"),
          box( width = 15,
               selectInput("univarNumericaldisc", "choisir la variable", choices = NULL)
          )),
        fluidRow(
          box(height = 460, width = 6,
              plotlyOutput("univarNumericalbarplot") 
          ),
          box(height = 460, width = 6,
              plotlyOutput("univarNumericalbarcumul")
          )),
        fluidRow(
          box(height = 460, width = 12,
              plotlyOutput("univarNumericalbarBmoust")
          )
        ),
        fluidRow(
          h3("Variable Numerique Continue"),
          box( width = 15,
               selectInput("univarNumericalcont", "choisir la variable", choices = NULL),
               sliderInput("bins", "Nombre de classes (K):", min = 1, max = 30, value = 10)
          )),
        fluidRow(
          box(height = 460, width = 6,
              plotlyOutput("univarNumericalHist") 
          ),
          box(height = 460, width = 6,
              plotlyOutput("univarNumericalCden")
          )),
      )
      # Générer la page  de bi var  uniquement lorsque l'utilisateur clique sur "Détails" 
    } else if(currentBoxView() == "bi_var_details") {
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
              plotlyOutput("variance")
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
        h1("Target Distribution"),
        fluidRow(
          box(width = 15,
              selectInput("distribution", "Categorielle",  choices = NULL)
          )),
        fluidRow(
          box(width = 15, height = "460",
              plotlyOutput("distributionPlot")
          )))
      # Générer la page  de splitting  uniquement lorsque l'utilisateur clique sur "Détails" 
    } else if (currentBoxView() == "sample_details") {
      fluidPage(
        h1("Détails pour Splitting")
        
      )
    }}
  )
  
  
  
  
  #------------------Uni variable------------------------------  
  ## var categorielle ----------  
  # Fonction pour créer le diagramme circulaire
  output$selectedUnivarCategorical <- renderPlotly({
    req(input$target, input$univarCategorical)
    pie_bivar <- plot_ly(
      labels = ~df[[input$target]],
      type = 'pie',
      domain = list(x = c(0.45, 1)),
      marker = list(colors = 'Set1'))
    pie_bivar})
  
  # Fonction pour créer le diagramme circulaire
  output$univarTargetVariablePlot <- renderPlotly({
    req(input$univarCategorical)
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
    createBarPlot(df, input$univarNumericaldisc, input$univarNumericaldisc, "Fréquences", "Fréquences ")
  })
  
  # Commande pour l'affichage du plot des fréquences cumulées
  output$univarNumericalbarcumul <- renderPlotly({
    req(input$univarNumericaldisc)  # Assurez-vous que la variable est sélectionnée
    createCumulativePlot(df[[input$univarNumericaldisc]], input$univarNumericaldisc, "Fréquences cumulées", "Fréquences cumulées ")
  })
  
  # Commande pour l'affichage du plot de Boîte à moustaches
  output$univarNumericalbarBmoust <- renderPlotly({
    req(input$univarNumericaldisc)  # Assurez-vous que la variable est sélectionnée
    createBoxplot(df, input$univarNumericaldisc, input$univarNumericaldisc, "Boîte à moustaches ", 0.3)
  })
  
  ## var num cont
  # Fonction réactive pour mettre à jour le nombre de classes---------------
  
  output$univarNumericalHist <- renderPlotly({
    variable <- input$univarNumericalcont
    bins <- input$bins
    
    if (is.null(variable)) return(NULL)
    
    p <- plot_ly(df, x = ~get(variable), type = "histogram", nbins = bins)
    
    # Personnalisation supplémentaire si nécessaire
    p <- p %>% layout(title = paste("Histogramme de", variable))
    
    p
  })
  
  # Observer réactif pour les changements dans 'bins' et 'univarNumericalcont'
  observeEvent(c(input$bins, input$univarNumericalcont), {
    output$univarNumericalHist <- renderPlotly({
      variable <- input$univarNumericalcont
      bins <- input$bins
      
      if (is.null(variable)) return(NULL)
      
      p <- plot_ly(df, x = ~get(variable), type = "histogram", nbins = bins)
      
      # Personnalisation supplémentaire si nécessaire
      p <- p %>% layout(title = paste("Histogramme de", variable))
      
      p
    })
  })
  
  
  # fonction courbe cumulative ---------------
  output$univarNumericalCden <- renderPlotly({
    createCumulativePlotCont(df$input$univarNumericalcont, "Fréquence Cumulative", "Variable X", "Courbe Cumulative")
  })
  #plotlyOutput("univarNumericalHist")plotlyOutput("univarNumericalCden")
  
  #------------------Bi variable-------------
  
  
  
  
  
  
  
  
  
  
}
shinyApp(ui, server)