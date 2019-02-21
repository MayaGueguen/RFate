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
##' @param name.simulation a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.soil a \code{data.frame} with 5 columns : PFG, type,
##' soil_contrib, soil_tol_min, soil_tol_max
##' @param mat.PFG.tol a \code{data.frame} with 5 columns : PFG, lifeStage,
##' soilResources, soil_tol
##' 
##' 
##' @details
##' 
##' The soil module allows the user to simulate a primary vegetation succession 
##' based on soil competition. \cr
##' 
##' Several parameters, given within \code{mat.PFG.soil}, are required for each
##' PFG in order to set up the soil competition :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. It should be either \code{H} 
##'   (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) for now}
##'   \item{soil_contrib}{a value between 0 and X corresponding to the nitrogen 
##'   value of the PFG (e.g. from Ellenberg)}
##'   \item{soil_tol_min}{the minimum nitrogen value tolerated by the PFG}
##'   \item{soil_tol_max}{the maximum nitrogen value tolerated by the PFG}
##' }
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' each PFG :
##' 
##' \describe{
##'   \item{SOIL_CONTRIB}{nitrogen contribution to the soil value of the PFG}
##'   \item{SOIL_LOW}{minimum nitrogen value tolerated by the PFG}
##'   \item{SOIL_HIGH}{maximum nitrogen value tolerated by the PFG}
##'   \item{ACTIVE_GERM}{proportion of seeds that will germinate for each soil
##'   condition (Low, Medium, High) :
##'   \itemize{
##'     \item woody species have little variation in germination rate : 90\% for
##'     Low and High conditions, 100\% for Medium condition
##'     \item herbaceous germinate less in richer soil : 80\% for Low, 100\% for
##'     Medium and 50\% for High conditions
##'   }
##'   }
##' }
##' 
##' A second file, \code{mat.PFG.tol}, can be given to define the importance of
##' the response of each PFG to each soil condition :
##' 
##' \describe{
##'   \item{PFG}{the name of the PFG concerned}
##'   \item{lifeStage}{the concerned life stage (Germinant, Immature, Mature)}
##'   \item{soilResources}{the concerned soil condition (Low, Medium, High)}
##'   \item{soil_tol}{the proportion of surviving individuals}
##' }
##' 
##' If these values are not given by the user, default rules are defined below :
##' 
##' \describe{
##'   \item{SOIL_TOL}{ defined for each life stage (Germinant, Immature, 
##'   Mature) and each soil condition (Low, Medium, High) :
##'   \itemize{
##'     \item (A) germinants are severely impacted by wrong soil conditions :
##'     \itemize{
##'       \item only 10\% of the cohorts survive in Low soil condition
##'       \item all cohorts die in High soil condition
##'     }
##'     \item (B) immatures are half impacted by wrong soil conditions :
##'     \itemize{
##'       \item 50\% of the cohorts survive in Low soil condition
##'       \item 40\% of the cohorts survive in High soil condition
##'     }
##'     \item (C) matures are little affected by wrong soil conditions :
##'     \itemize{
##'       \item 90\% of the cohorts survive in Low soil condition
##'       \item 80\% of the cohorts survive in High soil condition
##'     }
##'   }
##'   }
##' }
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/SOIL/} directory with the following 
##' parameters :
##' 
##' \itemize{
##'   \item NAME : name of the PFG
##'   \item SOIL_CONTRIB : the contribution (influence) of the PFG on the
##'   nitrogen soil value of the pixel
##'   \item SOIL_LOW : the lower value of nitrogen supported by the PFG
##'   \item SOIL_HIGH : the upper value of nitrogen supported by the PFG
##'   \item ACTIVE_GERM : the germination rates depending on soil conditions
##'   \cr \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}
##'   \item SOIL_TOL : the PFG soil tolerance table (in a single row). \cr 
##'   This is a vector of 9 numbers corresponding to the ability of the PFG to
##'   survive or not :
##'   \itemize{
##'     \item at different life stages \emph{(Germinant (Ge), Immature (Im), 
##'     Mature (Ma))}
##'     \item under different soil conditions \emph{(Low (L), Medium (M) or 
##'     High (H))}.
##'   }
##'   These parameters should be given in this order : GeL, GeM, GeH, ImL, ImM,
##'   ImH, MaL, MaM, MaH.
##'   \cr \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}
##' }
##' 
##' A \code{SOIL_COMPLETE_TABLE.csv} file summarizing information for all groups
##' into the \code{name.simulation/DATA/PFGS/} directory.  
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
##'                                                     , type = c("H", "H", "C")
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
  , mat.PFG.tol = NULL
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/SOIL/")
  
  ## CHECKS for parameter mat.PFG.soil
  if (.testParam_notDf(mat.PFG.soil))
  {
    .stopMessage_beDataframe("mat.PFG.soil")
  } else
  {
    if (nrow(mat.PFG.soil) == 0 || ncol(mat.PFG.soil) != 5)
    {
      .stopMessage_numRowCol("mat.PFG.soil", c("PFG", "type", "soil_contrib", "soil_tol_min", "soil_tol_max"))
    }
    if (ncol(mat.PFG.soil) == 5)
    {
      if (sum(colnames(mat.PFG.soil) == c("PFG", "type", "soil_contrib", "soil_tol_min", "soil_tol_max")) == 5)
      {
        mat.PFG.soil = mat.PFG.soil[ , c("PFG", "type", "soil_contrib", "soil_tol_min", "soil_tol_max")]
      } else
      {
        .stopMessage_columnNames("mat.PFG.soil", c("PFG", "type", "soil_contrib", "soil_tol_min", "soil_tol_max"))
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
    if (.testParam_notInChar(mat.PFG.soil$type, inList = c("H", "C", "P")))
    {
      .stopMessage_content("mat.PFG.soil$type", c("H", "C", "P"))
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
  ## CHECKS for parameter mat.PFG.tol
  if (!is.null(mat.PFG.tol))
  {
    if (.testParam_notDf(mat.PFG.tol))
    {
      .stopMessage_beDataframe("mat.PFG.tol")
    } else
    {
      if (nrow(mat.PFG.tol) == 0 || ncol(mat.PFG.tol) != 4)
      {
        .stopMessage_numRowCol("mat.PFG.tol", c("PFG", "lifeStage", "soilResources", "soil_tol"))
      }
      if (ncol(mat.PFG.tol) == 4)
      {
        if (sum(colnames(mat.PFG.tol) == c("PFG", "lifeStage", "soilResources", "soil_tol")) == 4)
        {
          mat.PFG.tol = mat.PFG.tol[ , c("PFG", "lifeStage", "soilResources", "soil_tol")]
        } else
        {
          .stopMessage_columnNames("mat.PFG.tol", c("PFG", "lifeStage", "soilResources", "soil_tol"))
        }
      }
      mat.PFG.tol$PFG = as.character(mat.PFG.tol$PFG)
      if (.testParam_notChar(mat.PFG.tol$PFG))
      {
        .stopMessage_beChar("mat.PFG.tol$PFG")
      }
      if (.testParam_notInChar(mat.PFG.tol$lifeStage, inList = c("Germinant", "Immature", "Mature")))
      {
        .stopMessage_content("mat.PFG.tol$lifeStage", c("Germinant", "Immature", "Mature"))
      }
      if (.testParam_notInChar(mat.PFG.tol$soilResources, inList = c("Low", "Medium", "High")))
      {
        .stopMessage_content("mat.PFG.tol$soilResources", c("Low", "Medium", "High"))
      }
      if (!is.numeric(mat.PFG.tol$soil_tol)) {
        .stopMessage_columnNumeric("mat.PFG.tol", c("soil_tol"))
      }
      if (length(which(is.na(mat.PFG.tol$soil_tol))) > 0) {
        .stopMessage_columnNoNA("mat.PFG.tol", c("soil_tol"))
      }
      if (sum(mat.PFG.tol$soil_tol %in% seq(0,10)) < nrow(mat.PFG.tol)){
        stop("Wrong type of data!\n Column `soil_tol` of `mat.PFG.tol` must contain values between 0 and 10")
      }
    }
  }
  
  #################################################################################################
  
  no.PFG = nrow(mat.PFG.soil)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.soil$PFG)
  
  ## GET SOIL CONTRIBUTION
  SOIL_CONTRIB = as.numeric(mat.PFG.soil$soil_contrib)
  
  ## GET SOIL TOLERANCE LIMITS
  SOIL_LOW = as.numeric(mat.PFG.soil$soil_tol_min)
  SOIL_HIGH = as.numeric(mat.PFG.soil$soil_tol_max)
  
  
  no.class = seq(min(round(mat.PFG.soil$soil_contrib))
                 , max(round(mat.PFG.soil$soil_contrib))
                 , 1)
  
  cat("\n ############## CLASS INFORMATIONS ############## \n")
  cat("\n Classes : ", no.class)
  cat("\n Number of classes : ", max(no.class))
  cat("\n Number of PFG within each class (contribution) : ", table(cut(mat.PFG.soil$soil_contrib
                                                                        , breaks = 1:max(no.class))))
  cat("\n")
  
  #################################################################################################
  
  ## GET GERMINATION RATE depending on soil conditions
  ##   = these rates should express a deviation from the
  ##     germination rate in optimal conditions (=100%)
  ##   = for each soil condition (Low, Medium, High)
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  ACTIVE_GERM = matrix(10, nrow = 3, ncol = no.PFG)
  ## woody species have little variation in germination rate depending on soil conditions
  ACTIVE_GERM[c(1,3), which(mat.PFG.soil$type %in% c("C", "P"))] = 9
  ## herbaceous germinate less in richer soil
  ACTIVE_GERM[1, which(mat.PFG.soil$type == "H")] = 8 ## low soil conditions
  ACTIVE_GERM[3, which(mat.PFG.soil$type == "H")] = 5 ## high soil conditions
  
  #################################################################################################
  
  ## GET SOIL TOLERANCE
  ##    = for each life stage (Germinant, Immature, Mature)
  ##    = for each soil condition (Low, Medium, High)
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  SOIL_TOL = matrix(10, nrow = 3 * 3, ncol = no.PFG)
  
  if (is.null(mat.PFG.tol))
  {
    SOIL_TOL[1, ] = 1 ## Germinant - Low soil conditions
    SOIL_TOL[3, ] = 0 ## Germinant - High soil conditions
    
    SOIL_TOL[4, ] = 5 ## Immature - Low soil conditions
    SOIL_TOL[6, ] = 4 ## Immature - High soil conditions
    
    SOIL_TOL[7, ] = 9 ## Mature - Low soil conditions
    SOIL_TOL[9, ] = 8 ## Mature - High soil conditions
  } else
  {
    for (ii in 1:nrow(mat.PFG.tol))
    {
      LS_res = paste0(mat.PFG.tol$lifeStage[ii], "_", mat.PFG.tol$soilResources[ii])
      ind_ii = switch(LS_res
                      , Germinant_Low = 1
                      , Germinant_Medium = 2
                      , Germinant_High = 3
                      , Immature_Low = 4
                      , Immature_Medium = 5
                      , Immature_High = 6
                      , Mature_Low = 7
                      , Mature_Medium = 8
                      , Mature_High = 9
      )
      SOIL_TOL[ind_ii, which(NAME == mat.PFG.tol$PFG[ii])] = mat.PFG.tol$soil_tol[ii]
    }
  }
  
  #################################################################################################
  
  names.params.list = mat.PFG.soil$PFG
  names.params.list.sub = c("NAME", "SOIL_CONTRIB"
                            , "SOIL_LOW", "SOIL_HIGH"
                            , "ACTIVE_GERM", "SOIL_TOL")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "SOIL_CONTRIB"
                           , "SOIL_LOW"
                           , "SOIL_HIGH"
                           , paste0("ACTIVE_GERM_for_", c("L", "M", "H"))
                           , paste0("SOIL_TOL_for_",
                                    c("GeL", "GeM", "GeH", "ImL", "ImM", "ImH", "MaL", "MaM", "MaH"))
  )
  
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
