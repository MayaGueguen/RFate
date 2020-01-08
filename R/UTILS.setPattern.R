### HEADER #####################################################################
##' @title Replace a pattern with a new within all parameter files of a 
##' \code{FATE-HD} simulation folder
##' 
##' @name .setPattern
##'
##' @author Maya Guéguen
##' 
##' @description This function scans all the files within a \code{FATE-HD} 
##' simulation folder to find a specific pattern and replace it with a new one
##' 
##' @param name.simulation a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param pattern.tofind a \code{string} that corresponds to the pattern to 
##' find
##' @param pattern.toreplace a \code{string} that corresponds to the pattern to 
##' replace
##'
## END OF HEADER ###############################################################


.setPattern = function(name.simulation
                       , pattern.tofind
                       , pattern.toreplace
                       
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  if (.testParam_notChar(pattern.tofind))
  {
    .stopMessage_beChar("pattern.tofind") 
  }
  if (.testParam_notChar(pattern.toreplace))
  {
    .stopMessage_beChar("pattern.toreplace") 
  }
  
  all.files = list.files(path = name.simulation
                         , pattern = ".txt$"
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  
  for (fi in all.files)
  {
    params.lines = readLines(fi)
    if (length(grep(pattern.tofind, params.lines)) > 0){
      params.lines = sub(pattern.tofind, pattern.toreplace, params.lines)
      cat(params.lines, sep = "\n", file = fi, append = FALSE)
      message(paste0("\n The parameter file ", fi, " has been successfully corrected !\n"))
    }
  }
}

