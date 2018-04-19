### HEADER #####################################################################
##' @title Create the skeleton folder for a \code{FATE-HD} simulation
##' 
##' @name PRE_FATE.skeletonDirectory
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a user-friendly directory
##' tree to run a \code{FATE-HD} simulation.
##'              
##' @param name.simulation a \code{string} that will be used as the main directory
##' and simulation name
##' 
##' @details 
##' 
##' \code{FATE-HD} requires only one input parameter, which is a file containing
##' the names of parameter files, which may themselves contain parameters or other
##' file names. The point is : the user could give names of files stored everywhere
##' on a machine, and does not have to put them all in one same place.
##' 
##' But as this is more practical, this function proposes a way to organize all
##' those files or parameter files that will or could be used by a \code{FATE-HD}
##' simulation.
##' 
##' The tree structure is detailed below :
##' 
##' \describe{
##'   \item{DATA}{this folder will contain all the data or parameters that are
##'   needed by the model
##'   \itemize{
##'     \item \strong{GLOBAL_PARAMS} : files containing global parameters for the simulation
##'     \item \strong{NAMESPACE_CONSTANTS} : files containing constants used in the model
##'     \item \strong{MASK} : all maps used in the model
##'     \item \strong{SCENARIO} : files containing information about changes in
##'     input data ( e.g. habitat suitability maps, disturbances maps, etc) 
##'     \item \strong{SAVE} : files containing information about times to save outputs
##'     \item \strong{PFGS} :
##'     \itemize{
##'       \item \strong{SUCC} : all the PFG life history parameter files
##'       \item \strong{DISP} : all the PFG dispersal parameter files
##'       \item \strong{DIST} : all the PFG-response to disturbances parameter files
##'       \item \strong{ENVSUIT} : all the PFG habitat suitability maps
##'     }
##'   }
##'   }
##'   \item{PARAM_SIMUL}{this folder will contain simulation files that can be
##'   given as input to the software}
##'   \item{RESULTS}{this folder will collect all the results produced by the
##'   software with a folder for each simulation}
##' }
##' 
##' 
##' @return A directory tree with folder to contain the parameter files, the
##' simulation files and the results.
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.skeletonDirectory = function(name.simulation = "FATE_simulation")
{
  if (is.na(name.simulation) || length(name.simulation) == 0){
    stop("No data given!\n (missing `name.simulation`)")
  } else if (!is.character(name.simulation)){
    stop("Wrong type of data!\n `name.simulation` must contain a character value")
  } else if (file.exists(name.simulation)) {
    ## do nothing if directory already exists
    warning("Directory already exists! (`", name.simulation, "`)")
    invisible(NULL)
  } else {
    ## the main simulation dir
    dir.create(name.simulation, showWarnings = FALSE)
    ## the DATA dir
    dir.create(file.path(name.simulation, "DATA"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "GLOBAL_PARAMS"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "NAMESPACE_CONSTANTS"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "MASK"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "SCENARIO"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "SAVE"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "SUCC"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "ENVSUIT"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "DIST"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "DISP"), showWarnings = FALSE)
    ## the simulation parameters dir
    dir.create(file.path(name.simulation, "PARAM_SIMUL"), showWarnings = FALSE)
    ## the RESULTS dir
    dir.create(file.path(name.simulation, "RESULTS"), showWarnings = FALSE)
    
    invisible(NULL)
    
    message(paste0("\n Your directory tree for your FATE-HD simulation (", name.simulation, ") is ready!\n"))
  }
}


