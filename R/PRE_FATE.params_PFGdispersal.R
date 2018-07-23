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
##' @return A \code{.txt} file per PFG into the \code{name.simulation/DATA/PFGS/DISP/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item DISPERS_DIST
##' }
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
##'                             , mat.PFG.disp = data.frame(PFG = c("PFG1", "PFG2", "PFG3")
##'                                                         , d50 = c(500, 500, 100)
##'                                                         , d99 = c(10000, 15000, 20000)
##'                                                         , ldd = c(100000, 50000, 100000)))
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGdispersal = function(
  name.simulation
  , mat.PFG.disp
){
  
  if (.testParam_notChar(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/PFGS/DISP/"))){
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  }
  if (.testParam_notDf(mat.PFG.disp))
  {
    stop("Wrong type of data!\n `mat.PFG.disp` must be a data.frame")
  } else
  {
    if (nrow(mat.PFG.disp) == 0 || ncol(mat.PFG.disp) != 4)
    {
      stop("Wrong dimension(s) of data!\n `mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)")
    }
    if (ncol(mat.PFG.disp) == 4)
    {
      if (sum(colnames(mat.PFG.disp) == c("PFG", "d50", "d99", "ldd")) == 4)
      {
        mat.PFG.disp = mat.PFG.disp[ , c("PFG", "d50", "d99", "ldd")]
      } else {
        stop("Wrong type of data!\n Column names of `mat.PFG.disp` must be `PFG`, `d50`, `d99` and `ldd`")
      }
    }
    if (length(unique(mat.PFG.disp$PFG)) < nrow(mat.PFG.disp)){
      stop("Wrong type of data!\n Column `PFG` of mat.PFG.disp` must contain different values")
    }
    if (!is.numeric(mat.PFG.disp$d50) ||
        !is.numeric(mat.PFG.disp$d99) ||
        !is.numeric(mat.PFG.disp$ldd)) {
      stop("Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
    }
    if (length(which(is.na(mat.PFG.disp$d50))) > 0 ||
        length(which(is.na(mat.PFG.disp$d99))) > 0 ||
        length(which(is.na(mat.PFG.disp$ldd))) > 0) {
      stop("Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must not contain NA values")
    }
  }
  
  params.list = lapply(1:nrow(mat.PFG.disp), function(x) {
    list(as.integer(mat.PFG.disp[x , c("d50", "d99", "ldd")]))
  })
  names.params.list = mat.PFG.disp$PFG
  names.params.list.sub = "DISPERS_DIST"

  for (i in 1:length(params.list)) {
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/PFGS/DISP/DISP_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
}
