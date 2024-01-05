#-------------- Uni var categ ---------------
# fonction visualiser les variable 
button.to.remove <- c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian")

#------------- Uni var analysis -----------

#------------- Numerical Var ----------

#-------------par variable -------------------------



visua_univar_data <- fonction(df,output){
  for(col_name in names(df)){
    if (is.numeric(df[[col_name]])|| is.character(df[[col_name]])) {
      
      # Calcul des effectifs
      effectifs <- reactive({table(data())})
      
      # Diagramme en colonnes
      output$colonnes <- renderPlot({
        barplot(df(), main = "Catégories ", 
                ylab="Effectifs", las = 2,
                names.arg = substr(names(df()), 1, 4))
        
      })
      
      # Diagramme en secteurs
      output$secteurs <- renderPlot({
        pie(df(), labels = substr(names(df()), 1, 4), 
            main = "Catégories Socioprofessionnelles", col=c())
      })
    }
      
      
} }






