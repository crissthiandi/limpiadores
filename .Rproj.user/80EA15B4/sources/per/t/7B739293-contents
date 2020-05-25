
limpiar_enviroment<-function(){
  #funcion que limpia el enviroment totalmente
  rm(list = ls())
}

limpiar_console <- function(){
  #limpia la consola
  cat("\014")
}

limpiar_plots<-function(){
  #para limpiar y antes checar usamos
  if(!is.null(dev.list())){
    #selecciona solo los entornos de Rstudio de la lista en uso
    dev.off(dev.list()["RStudioGD"])
  }
}

limpiar_todo<-function(){
  #funcion que limpia el enviroment totalmente
  rm(list = ls())
  #limpia la consola
  cat("\014")
  #para limpiar y antes checar usamos
  if(!is.null(dev.list())){
    #selecciona solo los entornos de Rstudio de la lista en uso
    dev.off(dev.list()["RStudioGD"])
  }
}
