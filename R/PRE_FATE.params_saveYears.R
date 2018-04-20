### HEADER #####################################################################
##' @title Create \emph{SAVE} parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_saveYears
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a parameter file containing
##' simulation years at which the \code{FATE-HD} software must save rasters of
##' PFG abundances or simulation objects.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param years.maps a \code{vector} of simulation years at which PFG abundance
##' maps will be saved
##' @param years.objects a \code{vector} of simulation years at which \code{FATE-HD}
##' simulation state will be saved
##' 
##' 
##' @details 
##' 
##' \code{FATE-HD} software allows the user to save two different types of outputs :
##' 
##' \describe{
##'   \item{Raster maps}{PFG maps can be saved for all specified simulation years.
##'   It includes maps per PFG per strata and summary maps per PFG for all height
##'   strata combined. \cr Raster format used is depending on input data format. It
##'   can be either \code{.img} or \code{.tif}.}
##'   \item{Model objects}{using \code{BOOST} library and its serialization functions,
##'   \code{FATE-HD} is able to save a simulation at a specific time. This object
##'   allows the user to restart a simulation from this precise state.}
##' }
##' 
##' 
##' @return Two \code{.txt} files into the \code{name.simulation/DATA/SAVE/}
##' directory with one line for each simulation year for which the raster maps /
##' \code{FATE-HD} object are to be saved.
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Namespace_constants parameter file
##' PRE_FATE.params_saveYears(name.simulation = "FATE_simulation"
##'                           , years.maps = c(100, 150, 200)
##'                           , years.objects = 200)
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_saveYears = function(
  name.simulation
  , years.maps = NULL
  , years.objects = NULL
){
  
  if (missing(name.simulation) ||
      !is.character(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/SAVE/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  }
  if ( (!is.null(years.maps) && !is.numeric(years.maps)) ||
       (!is.null(years.objects) && !is.numeric(years.objects))){
    stop("Wrong type of data!\n `years.maps` and/or `years.objects` must contain numeric values")
  }
  if (is.null(years.maps) && is.null(years.objects)){
    warning("Both `years.maps` and `years.objects` parameters are NULL. No parameter file will be created")
  } else
  {
    if (!is.null(years.maps))
    {
      params = lapply(years.maps, function(x) x)
      names(params) = rep("", length(params))
      .createParams(params.file = paste0(name.simulation, "/DATA/SAVE/SAVE_YEARS_maps.txt")
                    , params.list = params)
      file.lines = readLines(paste0(name.simulation, "/DATA/SAVE/SAVE_YEARS_maps.txt"))
      file.lines = file.lines[-c(1,2)]
      file.lines = gsub(" ", "", file.lines)
      cat(file.lines, sep = "\n", file = paste0(name.simulation, "/DATA/SAVE/SAVE_YEARS_maps.txt"), append = FALSE)
    }
    
    if (!is.null(years.objects))
    {
      params = lapply(years.objects, function(x) x)
      names(params) = rep("", length(params))
      .createParams(params.file = paste0(name.simulation, "/DATA/SAVE/SAVE_YEARS_objects.txt")
                    , params.list = params)
      file.lines = readLines(paste0(name.simulation, "/DATA/SAVE/SAVE_YEARS_objects.txt"))
      file.lines = file.lines[-c(1,2)]
      file.lines = gsub(" ", "", file.lines)
      cat(file.lines, sep = "\n", file = paste0(name.simulation, "/DATA/SAVE/SAVE_YEARS_objects.txt"), append = FALSE)
    }
  }
}

