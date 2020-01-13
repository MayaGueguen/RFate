### HEADER #####################################################################
##' @title Create \emph{DISPERSAL} parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_PFGdispersal
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' dispersal distances for each PFG (one file for each of them) used in the
##' dispersal module of \code{FATE-HD}.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.disp a \code{data.frame} with 4 columns : PFG, d50, d99, ldd
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} that
##' corresponds to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/DISP/} directory to store the results
##' 
##' 
##' @details
##' 
##' For each PFG, a dispersal module is available to disperse seeds
##' with a kernel. \cr
##' Dispersal distances are needed to quantify the amount of
##' seeds dispersed into 3 different concentric circles :
##' 
##' \describe{
##'   \item{d50}{the distance at which 50\% of seeds are dispersed}
##'   \item{d99}{the distance at which 49\% of seeds are dispersed}
##'   \item{ldd}{the long dispersal distance at which 1\% of seeds are dispersed}
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the \code{name.simulation/DATA/PFGS/DISP/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item DISPERS_DIST
##' }
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the folder
##' \code{name.simulation/DATA/PFGS/DISP/opt.folder.name/}
##' 
##' 
##' @keywords FATE, simulation, dispersal distance
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
##'                             , mat.PFG.disp = data.frame(PFG = c("PFG1", "PFG2", "PFG3")
##'                                                         , d50 = c(50, 50, 10)
##'                                                         , d99 = c(1000, 1500, 2000)
##'                                                         , ldd = c(10000, 5000, 10000)))
##' 
##' 
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##'   
##' ## Load example data
##' PNE_PARAM = .loadData("PNE_PARAM")
##' 
##' ## PNE_PARAM$disp : data.frame
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
##'                             , mat.PFG.disp = PNE_PARAM$disp)
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGdispersal = function(
  name.simulation
  , mat.PFG.disp
  , opt.folder.name = NULL
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/DISP/")
  
  if (.testParam_notDf(mat.PFG.disp))
  {
    .stopMessage_beDataframe("mat.PFG.disp")
  } else
  {
    if (nrow(mat.PFG.disp) == 0 || ncol(mat.PFG.disp) != 4)
    {
      .stopMessage_numRowCol("mat.PFG.disp", c("PFG", "d50", "d99", "ldd"))
    }
    if (ncol(mat.PFG.disp) == 4)
    {
      if (sum(colnames(mat.PFG.disp) == c("PFG", "d50", "d99", "ldd")) == 4)
      {
        mat.PFG.disp = mat.PFG.disp[ , c("PFG", "d50", "d99", "ldd")]
      } else
      {
        .stopMessage_columnNames("mat.PFG.disp", c("PFG", "d50", "d99", "ldd"))
      }
    }
    mat.PFG.disp$PFG = as.character(mat.PFG.disp$PFG)
    if (length(which(is.na(mat.PFG.disp$PFG))) > 0 ||
        length(unique(mat.PFG.disp$PFG)) < nrow(mat.PFG.disp)){
      stop("Wrong type of data!\n Column `PFG` of `mat.PFG.disp` must contain different values")
    }
    if (.testParam_notChar(mat.PFG.disp$PFG))
    {
      .stopMessage_beChar("mat.PFG.disp$PFG")
    }
    if (!is.numeric(mat.PFG.disp$d50) ||
        !is.numeric(mat.PFG.disp$d99) ||
        !is.numeric(mat.PFG.disp$ldd)) {
      .stopMessage_columnNumeric("mat.PFG.disp", c("d50", "d99", "ldd"))
    }
    if (length(which(is.na(mat.PFG.disp$d50))) > 0 ||
        length(which(is.na(mat.PFG.disp$d99))) > 0 ||
        length(which(is.na(mat.PFG.disp$ldd))) > 0) {
      .stopMessage_columnNoNA("mat.PFG.disp", c("d50", "d99", "ldd"))
    }
  }
  ## CHECKS for parameter opt.folder.name
  if (is.null(opt.folder.name)){
    opt.folder.name = ""
  } else if (!is.null(opt.folder.name) && !is.character(opt.folder.name)){
    warning("As `opt.folder.name` does not contain character value, it will be ignored")
    opt.folder.name = ""
  } else if (nchar(opt.folder.name) > 0){
    opt.folder.name = paste0(opt.folder.name, "/")
    dir.create(paste0(name.simulation, "/DATA/PFGS/DISP/", opt.folder.name))
  } else {
    opt.folder.name = ""
  }
  
  #################################################################################################
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.disp$PFG)

  ## GET DISPERSAL DISTANCES
  DISPERS_DIST = mat.PFG.disp[ , c("d50", "d99", "ldd"), drop = F]
  
#################################################################################################
  
  names.params.list = mat.PFG.disp$PFG
  names.params.list.sub = c("NAME", "DISPERS_DIST")
  
  params.csv = mat.PFG.disp
  colnames(params.csv) = c("NAME"
                           , paste0("DISPERS_DIST_", c("d50", "d99", "ldd")))
  for (i in grep("DIST", colnames(params.csv))) params.csv[, i] = as.integer(params.csv[,i])
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == "", "", sub("/$", "_", opt.folder.name))
                              , "DISP_COMPLETE_TABLE.csv")
              , row.names = F
              , col.names = T)
  
  #################################################################################################
  
  params.list = lapply(1:nrow(mat.PFG.disp), function(x) {
    lapply(names.params.list.sub, function(y) {
      val = get(y)
      if (is.null(nrow(val))){
        val = val[x]
      } else {
        val = as.integer(val[x, ])
      }
      return(val)
    })
  })
  
  for (i in 1:length(params.list)) {
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/PFGS/DISP/"
                                       , opt.folder.name
                                       , "DISP_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }

  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG dispersal parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
}
