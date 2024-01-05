library(tidyverse)
library(plotly)


button.to.remove <- c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian")

#------------- analyse Uni var  -----------

#-------------  var Numeri ----------


get.boxplot <- function(dataset, x.name){
  if (x.name == ""){
    return(NULL)
  }
  #cree 
  title <- paste0("Variance of ", x.name)
  fig <- plot_ly(dataset , y = dataset[[x.name]], type = "box")
  fig <- layout(fig, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(title = list(text = x.name)), title = list(text = title))
  return(config(fig, modeBarButtonsToRemove = button.to.remove, displaylogo = F))
}

get.histo <- function(dataset, x.name){
  fig <- plot_ly(dataset, y = dataset[[x.name]], name = "", type = "histogram")
  title <- paste0("Histo of ", x.name)
  fig <- layout(fig, yaxis = list(title = list(text = x.name)), title = list(text = title))
  return(config(fig, modeBarButtonsToRemove = button.to.remove, displaylogo = F))
}

#--------- Categorielle  Var ----------

get.pie <- function(dataset, x.name){
  fig <- plot_ly(dataset, labels = dataset[[x.name]], type = 'pie')
  fig <- fig %>% layout(title = paste0("Proportion de  ", x.name), 
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(config(fig, modeBarButtonsToRemove = button.to.remove, displaylogo = F))
}








