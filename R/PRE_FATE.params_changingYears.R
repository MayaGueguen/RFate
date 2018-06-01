### HEADER #####################################################################
##' @title Create \emph{SCENARIO} parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_changingYears
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a parameter file containing
##' simulation years at which the \code{FATE-HD} software must save rasters of
##' PFG abundances or simulation objects.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param mat.changing a 
##' @param opt.folder.name a \code{string} taht corresponds to the name of the folder
##' that will be created into the \code{name.simulation/DATA/SCENARIO/} directory to
##' store the results (\emph{optional})
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
##' ## Create a Changing_times parameter file
##' PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
##'                               , type.changing = "DIST"
##'                               , mat.changing = data.frame(year = c(50,50,80,80)
##'                                                           , order = c(1,2,1,2)
##'                                                           , file.name = c("MASK_DIST1_50.tif"
##'                                                                           , "MASK_DIST2_50.tif"
##'                                                                           , "MASK_DIST1_80.tif"
##'                                                                           , "MASK_DIST2_80.tif")))
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_changingYears = function(
  name.simulation
  , type.changing
  , mat.changing
  , opt.folder.name = NULL
){
  
  if (missing(name.simulation) ||
      !is.character(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/SCENARIO/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  }
  if (missing(type.changing) ||
      is.na(type.changing) ||
      is.null(type.changing) ||
      !(type.changing %in% c("MASK","HS","DIST"))){
    stop("Wrong type of data!\n `type.changing` must be either `MASK`, `HS` (habitat suitability) or `DIST`")
  }
  if (missing(mat.changing) || !is.data.frame(mat.changing))
  {
    stop("Wrong type of data!\n `mat.changing` must be a data.frame")
  }
  if (nrow(mat.changing) == 0 || ncol(mat.changing) != 3)
  {
    stop(paste0("Wrong dimension(s) of data!\n `mat.changing` does not have "
                , "the appropriate number of rows (>0) or columns (year, order, file.name)"))
  }
  if (ncol(mat.changing) == 3)
  {
    if (sum(colnames(mat.changing) == c("year", "order", "file.name")) == 3)
    {
      mat.changing = mat.changing[ , c("year", "order", "file.name")]
    } else {
      stop(paste0("Wrong type of data!\n Column names of `mat.changing` must be "
                  , "`year`, `order` and `file.name`"))
    }
  }
  if (!is.numeric(mat.changing$year) ||
      !is.numeric(mat.changing$order)) {
    stop("Wrong type of data!\n Columns `year` and `order` of `mat.changing` must contain numeric values")
  }
  if (length(which(is.na(mat.changing$year))) > 0 ||
      length(which(is.na(mat.changing$order))) > 0 ||
      length(which(is.na(mat.changing$file.name))) > 0) {
    stop("Wrong type of data!\n Columns `year`, `order` and `file.name` of `mat.changing` must not contain NA values")
  }
  if (length(unique(table(mat.changing$year, mat.changing$order))) > 1) {
    stop(paste0("Wrong type of data!\n Columns `year` and `order` are not balanced\n"
                , " All combinations must be represented"))
  }
  
  if (is.null(opt.folder.name)){
    opt.folder.name = ""
  } else if (!is.null(opt.folder.name) && !is.character(opt.folder.name)){
    warning("As `opt.folder.name` does not contain character value, it will be ignored")
    opt.folder.name = ""
  } else if (nchar(opt.folder.name) > 0){
    opt.folder.name = paste0(opt.folder.name, "/")
    dir.create(paste0(name.simulation, "/DATA/SCENARIO/", opt.folder.name))
  } else {
    opt.folder.name = ""
  }
  
  ### CREATE changing_times.txt file
  params = lapply(sort(unique(mat.changing$year)), function(x) x)
  names(params) = rep("", length(params))
  
  file.name = paste0(name.simulation, "/DATA/SCENARIO/", opt.folder.name, type.changing, "_changing_times.txt")
  .createParams(params.file = file.name
                , params.list = params)
  file.lines = readLines(file.name)
  file.lines = file.lines[-c(1,2)]
  file.lines = gsub(" ", "", file.lines)
  cat(file.lines, sep = "\n", file = file.name, append = FALSE)
  
  ### CREATE changing_masks.txt files
  for(y in sort(unique(mat.changing$year)))
  {
    changing.names = mat.changing[which(mat.changing$year == y),]
    changing.names = changing.names$file.name[order(changing.names$order)]
    
    params = lapply(changing.names, function(x) x)
    names(params) = rep("", length(params))
    
    file.name = paste0(name.simulation, "/DATA/SCENARIO/", opt.folder.name, type.changing, "_changing_masks_t", y, ".txt")
    .createParams(params.file = file.name
                  , params.list = params)
    file.lines = readLines(file.name)
    file.lines = file.lines[-c(1,2)]
    file.lines = gsub(" ", "", file.lines)
    cat(file.lines, sep = "\n", file = file.name, append = FALSE)
  }
}
