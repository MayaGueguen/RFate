### HEADER #####################################################################
##' @title Shiny application to apply \code{RFate} functions
##' 
##' @name SHINY_FATE.run
##'
##' @author Maya Guéguen
##' 
# @date 21/03/2018
##' 
##' @description This SHINY application allows to use all the \code{RFate} 
##' functions (PRE and POST FATE), from the building of PFG to the treatment of
##' FATE output files.
##'              
##' 
##' 
## @details 
## 
## This function allows one to obtain a distance matrix between species, based
## on two types of distance information :
##' 
##'  
##' @keywords shiny application, interface, GUI
##'  
##' @export
##' 
##' @importFrom shiny runApp
##' 
## END OF HEADER ###############################################################


SHINY_FATE.run = function(){
  
  appDir <- system.file("shinyApp", package = "RFate")
  if (appDir == "") {
    stop("Could not find shinyApp directory. Try re-installing `RFate`.", call. = FALSE)
  }
  
  runApp(appDir, display.mode = "normal")
}

