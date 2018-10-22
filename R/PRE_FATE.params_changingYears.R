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
##' @param type.changing a \code{string} to choose the concerned module : \cr
##' succession (\emph{MASK}), habitat suitability (\emph{HS}) or disturbances 
##' (\emph{DIST})
##' @param mat.changing a \code{data.frame} with 3 columns : year, order, file.name
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} that corresponds 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/SCENARIO/} directory to store the results
##' 
##' 
##' @details 
##' 
##' Several modules of the \code{FATE-HD} software allow the user to simulate
##' changes over time :
##' 
##' \describe{
##'   \item{succession}{the CORE module is based on a raster mask, \strong{with 
##'   either 0 or 1} within each pixel, 1 corresponding to the cells in which 
##'   the PFG can try to colonize. 
##'   The available pixels can change through time, to simulate habitat loss 
##'   (e.g. urbanization) or gain (e.g. glacial retreat).}
##'   \item{habitat suitability}{\strong{if this MODULE is activated} within the 
##'   \emph{Global_parameters} file, PFG colonization depends on maps given for 
##'   each PFG within the \emph{Simul_parameters} file with the \code{PFG_HAB_MASK}
##'   flag. \cr These maps must contain \strong{values between 0 and 1} 
##'   corresponding to the probability of presence of the group in each pixel. 
##'   These probabilities can change through time, as they often come from 
##'   Species Distribution Models (SDM) that can be based for example on climatic
##'   variables.}
##'   \item{disturbances}{\strong{if this MODULE is activated} within the 
##'   \emph{Global_parameters} file, each disturbance relies on a raster given  
##'   within the \emph{Simul_parameters} file with the \code{DIST_MASK}
##'   flag. \cr
##'   As for succession, this mask is filled \strong{with either 0 or 1} to 
##'   define where the perturbation occurs. The impacted pixels can also change 
##'   through time (e.g. change in forestry practices, expansion of grazing areas, 
##'   etc). \cr \cr}
##' }
##' 
##' 
##' These changes require two types of information, that must be presented within
##' two different files :
##' 
##' \describe{
##'   \item{Years of change \cr \code{column} year}{all simulation years at which
##'    the raster files of a specific module (succession, habitat suitability, 
##'    disturbance) will be changed. To each given year must correspond a file 
##'    with the names of the new maps.}
##'   \item{New maps / mask \cr \code{column} file.name}{for a specific year of 
##'   change, the names of the new raster files, always in the same order. \cr 
##'   It can be either \code{.img} or \code{.tif}.}
##'   \item{Order of the filenames \cr \code{column} order}{\code{integer} to be 
##'   sure to always give the raster maps in the same order.}
##' }
##' 
##' 
##' @return Several \code{.txt} files into the \code{name.simulation/DATA/SCENARIO/} :
##' \itemize{
##'   \item \code{type.changing_changing_times.txt} : one line for each simulation 
##'   year 
##'   \item \code{type.changing_changing_masks_t}...\code{.txt} : one line for each
##'   new raster file \cr \cr
##' }
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the folder
##' \code{name.simulation/DATA/SCENARIO/opt.folder.name/}
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
  
  .testParam_existFolder(name.simulation, "DATA/SCENARIO/")
  
  if (.testParam_notInChar(type.changing, inList = c("MASK","HS","DIST")))
  {
    .stopMessage_content("type.changing", c("MASK","HS","DIST"))
  }
  if (.testParam_notDf(mat.changing))
  {
    .stopMessage_beDataframe("mat.changing")
  }
  if (nrow(mat.changing) == 0 || ncol(mat.changing) != 3)
  {
    .stopMessage_numRowCol("mat.changing", c("year", "order", "file.name"))
  }
  if (ncol(mat.changing) == 3)
  {
    if (sum(colnames(mat.changing) == c("year", "order", "file.name")) == 3)
    {
      mat.changing = mat.changing[ , c("year", "order", "file.name")]
    } else {
      .stopMessage_columnNames("mat.changing", c("year", "order", "file.name"))
    }
  }
  if (!is.numeric(mat.changing$year) ||
      !is.numeric(mat.changing$order)) {
    .stopMessage_columnNumeric("mat.changing", c("year", "order"))
  }
  if (length(which(is.na(mat.changing$year))) > 0 ||
      length(which(is.na(mat.changing$order))) > 0 ||
      length(which(is.na(mat.changing$file.name))) > 0) {
    .stopMessage_columnNoNA("mat.changing", c("year", "order", "file.name"))
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
