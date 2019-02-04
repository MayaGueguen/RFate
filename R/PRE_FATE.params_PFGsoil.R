### HEADER #####################################################################
##' @title Create \emph{SOIL} parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_PFGsoil
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' soil contribution and tolerance for each PFG (one file for each of them) 
##' used in the soil module of \code{FATE-HD}.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.soil a \code{data.frame} with 5 columns : PFG, soil_contrib, 
##' soil_tol_min, soil_tol_max
##' @param no.class an \code{integer} that corresponds to the number of soil
##' classes. Default value is the maximum value of soil_tol_max in
##' \code{mat.PFG.soil}
##' 
##' 
##' @details
##' 
##' The soil module allows the user to simulate a primary vegetation succession 
##' based on soil competition. \cr
##' Several parameters are required for each PFG in order to set up the soil 
##' competition :
##' 
##' \describe{
##'   \item{soil_contrib}{a value between 0 and X corresponding to the nitrogen 
##'   value of the PFG (e.g. from Ellenberg)}
##'   \item{soil_tol_min}{the minimum nitrogen value tolerated by the PFG}
##'   \item{soil_tol_max}{the maximum nitrogen value tolerated by the PFG}
##' }
##' 
##' 
##' @return A \code{.txt} file per PFG into the \code{name.simulation/DATA/PFGS/SOIL/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item SOIL_CONTRIB
##'   \item SOIL_TOL
##' }
##' 
##' 
##' @keywords FATE, simulation, soil tolerance, nitrogen
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG soil parameter files
##' PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
##'                         , mat.PFG.soil = data.frame(PFG = c("PFG1", "PFG2", "PFG3")
##'                                                     , soil_contrib = c(2.5, 3, 4.8)
##'                                                     , soil_tol_min = c(2, 3, 3)
##'                                                     , soil_tol_max = c(3, 3, 6)))
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsoil = function(
  name.simulation
  , mat.PFG.soil
  , no.class = max(mat.PFG.soil$soil_tol_max)
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/SOIL/")
  
  if (.testParam_notDf(mat.PFG.soil))
  {
    .stopMessage_beDataframe("mat.PFG.soil")
  } else
  {
    if (nrow(mat.PFG.soil) == 0 || ncol(mat.PFG.soil) != 4)
    {
      .stopMessage_numRowCol("mat.PFG.soil", c("PFG", "soil_contrib", "soil_tol_min", "soil_tol_max"))
    }
    if (ncol(mat.PFG.soil) == 4)
    {
      if (sum(colnames(mat.PFG.soil) == c("PFG", "soil_contrib", "soil_tol_min", "soil_tol_max")) == 4)
      {
        mat.PFG.soil = mat.PFG.soil[ , c("PFG", "soil_contrib", "soil_tol_min", "soil_tol_max")]
      } else
      {
        .stopMessage_columnNames("mat.PFG.soil", c("PFG", "soil_contrib", "soil_tol_min", "soil_tol_max"))
      }
    }
    mat.PFG.soil$PFG = as.character(mat.PFG.soil$PFG)
    if (length(which(is.na(mat.PFG.soil$PFG))) > 0 ||
        length(unique(mat.PFG.soil$PFG)) < nrow(mat.PFG.soil)){
      stop("Wrong type of data!\n Column `PFG` of `mat.PFG.soil` must contain different values")
    }
    if (.testParam_notChar(mat.PFG.soil$PFG))
    {
      .stopMessage_beChar("mat.PFG.soil$PFG")
    }
    if (!is.numeric(mat.PFG.soil$soil_contrib) ||
        !is.numeric(mat.PFG.soil$soil_tol_min) ||
        !is.numeric(mat.PFG.soil$soil_tol_max)) {
      .stopMessage_columnNumeric("mat.PFG.soil", c("soil_contrib", "soil_tol_min", "soil_tol_max"))
    }
    if (length(which(is.na(mat.PFG.soil$soil_contrib))) > 0 ||
        length(which(is.na(mat.PFG.soil$soil_tol_min))) > 0 ||
        length(which(is.na(mat.PFG.soil$soil_tol_max))) > 0) {
      .stopMessage_columnNoNA("mat.PFG.soil", c("soil_contrib", "soil_tol_min", "soil_tol_max"))
    }
    if (sum(mat.PFG.soil$soil_tol_min > mat.PFG.soil$soil_contrib) > 0){
      stop("Wrong type of data!\n Column `soil_tol_min` of `mat.PFG.soil` must contain values equal or inferior to `soil_contrib`")
    }
    if (sum(mat.PFG.soil$soil_tol_max < mat.PFG.soil$soil_contrib) > 0){
      stop("Wrong type of data!\n Column `soil_tol_max` of `mat.PFG.soil` must contain values equal or superior to `soil_contrib`")
    }
  }
  
  #################################################################################################
  
  no.PFG = nrow(mat.PFG.soil)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.soil$PFG)
  
  ## GET SOIL CONTRIBUTION
  SOIL_CONTRIB = as.numeric(mat.PFG.soil$soil_contrib)
  
  ## GET SOIL TOLERANCE
  ##    = for each life stage (Germinant, Immature, Mature)
  ##    = and for each class
  ## 0 = non tolerant
  ## 1 = tolerant
  SOIL_TOL = matrix(0, nrow = 3 * no.class, ncol = no.PFG)
  
  for (i in 1:no.PFG)
  {
    ind_tol = seq(1, no.class, 1)
    ind_tol = ifelse(ind_tol >= mat.PFG.soil$soil_tol_min[i] & ind_tol <= mat.PFG.soil$soil_tol_max[i]
                     , 1, 0)
    SOIL_TOL[, i] = rep(ind_tol, 3)
  }
  
  # ### All woody PFGs are tolerant to rich soils when they are mature
  # #SOIL_TOL["Ma3",which(dat$type=="P")] <- 1
  # #SOIL_TOL["Ma3",which(dat$type=="C")] <- 1
  
  # ### All woody PFGs are ALSO tolerant to rich soils when they are Immature
  # #SOIL_TOL["Im3",which(dat$type=="P")] <- 1
  # #SOIL_TOL["Im3",which(dat$type=="C")] <- 1
  
  # ### All woody PFGs are ALWAYS tolerant to rich soils 
  # #SOIL_TOL["Ge3",which(dat$type=="P")] <- 1
  # #SOIL_TOL["Ge3",which(dat$type=="C")] <- 1
  
  cat("\n ############## CLASS INFORMATIONS ############## \n")
  cat("\n Number of classes : ", no.class)
  cat("\n Number of PFG within each class (contribution) : ", table(cut(mat.PFG.soil$soil_contrib, breaks = 1:no.class)))
  cat("\n")
  
  #################################################################################################
  
  names.params.list = mat.PFG.soil$PFG
  names.params.list.sub = c("NAME", "SOIL_CONTRIB", "SOIL_TOL")

  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "SOIL_CONTRIB"
                           , paste0("SOIL_TOL_for_",
                                    as.vector(sapply(c("Ge", "Im", "Ma")
                                                     , function(x) paste0(x, 1:no.class)))
                           ))
  
  write.table(params.csv
              , file = paste0(name.simulation, "/DATA/PFGS/SOIL_COMPLETE_TABLE.csv")
              , row.names = F
              , col.names = T)
  
  #################################################################################################
  
  params.list = lapply(1:no.PFG, function(x) {
    lapply(names.params.list.sub, function(y) {
      val = get(y)
      if (is.null(nrow(val))){
        val = val[x]
      } else {
        val = val[, x]
      }
      return(val)
    })
  })

  for (i in 1:length(params.list)) {
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/PFGS/SOIL/SOIL_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }

  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG soil parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
}
