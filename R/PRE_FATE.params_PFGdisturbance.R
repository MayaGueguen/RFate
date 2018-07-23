### HEADER #####################################################################
##' @title Create \emph{DISTURBANCE} parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_PFGdisturbance
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' response to disturbance parameters for each PFG (one file for each of them) 
##' used in the disturbance module of \code{FATE-HD}.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.dist a \code{data.frame} with at least 4 columns : \cr name, 
##' responseStage, KilledIndiv_[...], ResproutIndiv_[...] (see \code{Details})
##' @param mat.PFG.succ a \code{data.frame} with at least 6 columns : \cr
##' PFG, type, MATURITY, LONGEVITY, STRATA and CHANG_STR_AGES_to_str_3. \cr
##' Such an object can be obtained with the \code{PRE_FATE.params_PFGsuccession}
##'  function.
##' 
##' 
##' @details
##' 
##' The disturbance module of \code{FATE-HD} allows the user to simulate spatial
##' perturbation(s) that will impact each PFG in terms of resprouting and mortality
##' on the different response stages \emph{(!currently 4, not yet editable!)}. \cr
##' Several parameters are required for each PFG in order to set up these responses :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. It should be either \code{H} 
##'   (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) for now}
##'   \item{MATURITY}{the age from which the PFG can reproduce}
##'   \item{LONGEVITY}{the maximum or average lifespan of the PFG}
##'   \item{STRATA}{the maximum height stratum that the PFG can reach}
##'   \item{CHANG_STR_AGES}{at what age each PFG goes into the upper stratum}
##' }
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for each
##' PFG :
##' 
##' \describe{
##'   \item{PROP_KILLED}{ = the proportion of propagules killed by each 
##'   disturbance \cr \cr
##'   It is currently set to 0 for all PFG and disturbances.
##'   }
##'   \item{BREAK_AGE}{ = each PFG can respond to a disturbance in several 
##'   different ways \cr \emph{(!currently 4, not yet editable!)} that depend 
##'   on the PFG age \cr
##'    = ages at which each PFG changes of response stage \cr \cr
##'    
##'    These response classes are the same for all disturbances for each PFG. \cr
##'    These ages are defined according to the maturity and longevity of the PFG,
##'    its life form and its ability to reach the 3rd height stratum :
##'    \itemize{
##'      \item age from class 1 to 2 :
##'      \itemize{
##'        \item \code{maturity - 2} for herbaceous
##'        \item \code{1} for chamaephytes and phanerophytes
##'      }
##'      \item age from class 2 to 3 : \code{min(CHANG_STR_AGES_to_str_3, maturity)}
##'      \item age from class 3 to 4 : \code{min(CHANG_STR_AGES_to_str_3, longevity - 2)}
##'    }
##'    Some corrections are made for short-living plants (annuals, biennials...) :
##'    as they die after 1 or 2 years, they are not affected differently according
##'    to life stages. All ages are the same and correspond to their longevity.
##'   }
##'   \item{RESPR_AGE}{ = when subject to a perturbation, each PFG can either stay 
##'   undisturbed, be killed, or resprout \emph{(in years)} \cr
##'    = ages at which each PFG will be rejuvenated by a disturbance \cr \cr
##'   It does not impact dead inviduals, only living ones. \cr
##'   It is defined according to the maturity and longevity of the PFG, and its 
##'   ability to reach the 3rd height stratum :
##'   \itemize{
##'     \item individuals within 1st and 2nd response stage are too young to resprout
##'     \item individuals within 3rd response stage : \cr
##'     \code{min(CHANG_STR_AGES_to_str_3, maturity - 2)}
##'     \item individuals within 4th response stage : \code{longevity - 2}
##'     \item short-living plants (annuals, biennials...) always start back at 0
##'   }
##'   }
##'   \item{FATES}{ = proportion of killed or resprouting individuals \cr
##'    = for each disturbance and for each response stage   
##'   }
##'   \item{ACTIVATED_SEED}{ = the proportion of seeds activated by each 
##'   disturbance \cr \cr
##'   It is currently set to 0 for all PFG and disturbances.}
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the \code{name.simulation/DATA/PFGS/DIST/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item PROP_KILLED : the proportion of propagules killed by each disturbance \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)}
##'   \item BREAK_AGE : the age when each PFG changes of response stage \emph{(in years)}
##'   \item RESPR_AGE : the PFG resprouting age table (in a single row) \cr
##'   This is a vector of \code{no.DIST * no.responseStages} numbers \emph{(in years)}
##'   corresponding to the age at which each living PFG can be rejuvenated (younger 
##'   than the actual one) :
##'   \itemize{
##'     \item at different response stages \emph{(!currently 4, not yet editable!)}
##'     \item for each disturbance.
##'   }
##'   \item FATES : the PFG disturbance response table (in a single row) \cr
##'   This is a vector of \code{no.DIST * no.responseStages * 2} numbers \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)} \cr
##'   corresponding to the proportion of individuals :
##'   \itemize{
##'     \item that will be killed or resprout
##'     \item at different response stages \emph{(!currently 4, not yet editable!)}
##'     \item for each disturbance.
##'   }
##'   \item ACTIVATED_SEED : the proportion of seeds activated by each disturbance \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)} \cr \cr
##' }
##' 
##' A \code{DIST_COMPLETE_TABLE.csv} file summarizing information for all groups into the
##' \code{name.simulation/DATA/PFGS/} directory.
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG succession parameter files
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                             , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                         , type = c("C", "C", "H", "H", "P", "P")  
##'                                                         , height = c(10, 250, 36, 68, 1250, 550)
##'                                                         , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                         , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                         , dispersal = 1
##'                                                         , light = c(4, 6, 3, 6, 5, 5)))
##'                                                         
##' ## Create PFG disturbance parameter files
##' PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
##'                                , mat.PFG.dist = data.frame(name = rep(c("DIST1","DIST2"), each = 4)
##'                                                            , responseStage = rep(1:4, 2)
##'                                                            #, KilledPropagule_H = 0
##'                                                            #, KilledPropagule_C = 0
##'                                                            #, KilledPropagule_P = 0
##'                                                            , KilledIndiv_H = c(0,0,0,0,1,1,0,0)
##'                                                            , KilledIndiv_C = c(0,10,10,10,1,1,0,0)
##'                                                            , KilledIndiv_P = c(10,10,10,10,10,0,0,0)
##'                                                            , ResproutIndiv_H = c(0,0,9,10,0,0,5,1)
##'                                                            , ResproutIndiv_C = c(0,0,0,0,0,0,5,1)
##'                                                            , ResproutIndiv_P = c(0,0,0,0,0,0,0,0)))
##'                                                            
##' 
##' @export
##' 
##' @importFrom utils read.table write.table
##' @importFrom raster extension
##'
## END OF HEADER ###############################################################



PRE_FATE.params_PFGdisturbance = function(
  name.simulation
  , mat.PFG.dist
  , mat.PFG.succ = paste0(name.simulation, "/DATA/PFGS/SUCC_COMPLETE_TABLE.csv")
){
  
  if (.testParam_notChar(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/PFGS/DIST/"))){
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  }
  
  ## CHECKS for mat.PFG.succ parameter
  isDataFrame = is.data.frame(mat.PFG.succ)
  isCharacter = is.character(mat.PFG.succ)
  if (!isDataFrame && !isCharacter)
  {
    stop(paste0("Wrong type of data!\n `mat.PFG.succ` must be either :\n"
                , " ==> an existing `.csv` or `.txt` filename with a header and space separator"
                , " ==> a data.frame"))
  } else if (isCharacter && 
             (!file.exists(mat.PFG.succ) || !(extension(mat.PFG.succ) %in% c(".csv", ".txt"))))
  {
    stop(paste0("Wrong type of data!\n `mat.PFG.succ` must be an existing `.csv` or `.txt` "
                , "filename with a header and space separator"))
  } else if (isCharacter &&
             file.exists(mat.PFG.succ) && 
             (extension(mat.PFG.succ) %in% c(".csv", ".txt")))
  {
    mat.PFG.succ = read.table(file = mat.PFG.succ, header = T, sep = " ")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) < 6)
  {
    stop(paste0("Wrong dimension(s) of data!\n `mat.PFG.succ` does not have "
                , "the appropriate number of rows (>0) or columns "
                , "(at least NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_3)"))
  }
  if (ncol(mat.PFG.succ) >= 6)
  {
    if (sum(colnames(mat.PFG.succ) %in% c("NAME", "TYPE", "MATURITY", "LONGEVITY", "STRATA", "CHANG_STR_AGES_to_str_3")) < 6)
    {
      stop(paste0("Wrong type of data!\n Column names of `mat.PFG.succ` must contain "
                  , "`NAME`, `TYPE`, `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3`"))
    }
  }
  if (length(which(is.na(mat.PFG.succ$NAME))) > 0 ||
      length(unique(mat.PFG.succ$NAME)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `NAME` of `mat.PFG.succ` must contain different values and no NA values")
  }
  if (sum(mat.PFG.succ$TYPE %in% c("H", "C", "P")) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `TYPE` of `mat.PFG.succ` must contain values such as `H`, `C` or `P`")
  }
  if (!is.numeric(mat.PFG.succ$MATURITY) ||
      !is.numeric(mat.PFG.succ$LONGEVITY) ||
      !is.numeric(mat.PFG.succ$STRATA) ||
      !is.numeric(mat.PFG.succ$CHANG_STR_AGES_to_str_3)) {
    stop(paste0("Wrong type of data!\n Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` "
                , "of `mat.PFG.succ` must contain numeric values"))
  }
  if (length(which(is.na(mat.PFG.succ$MATURITY))) > 0 ||
      length(which(is.na(mat.PFG.succ$LONGEVITY))) > 0 ||
      length(which(is.na(mat.PFG.succ$STRATA))) > 0 ||
      length(which(is.na(mat.PFG.succ$CHANG_STR_AGES_to_str_3))) > 0) {
    stop(paste0("Wrong type of data!\n Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` "
                , "of `mat.PFG.succ` must not contain NA values"))
  }
  
  ## CHECKS for mat.PFG.dist parameter
  if (.testParam_notDf(mat.PFG.dist))
  {
    stop("Wrong type of data!\n `mat.PFG.dist` must be a data.frame")
  }
  if (nrow(mat.PFG.dist) == 0 || ncol(mat.PFG.dist) < 2)
  {
    stop(paste0("Wrong dimension(s) of data!\n `mat.PFG.dist` does not have "
                , "the appropriate number of rows (>0) or columns (at least `name` and `responseStage`)"))
  }
  if (ncol(mat.PFG.dist) >= 1)
  {
    if (sum(colnames(mat.PFG.dist) %in% c("name", "responseStage")) < 2)
    {
      stop(paste0("Wrong type of data!\n Column names of `mat.PFG.dist` must contain `name` and `responseStage`"))
    }
    # if (sum(unique(mat.PFG.dist$name) %in% mat.PFG.succ$NAME) < length(unique(mat.PFG.dist$name)))
    # {
    #   warning(paste0("Column `name` of `mat.PFG.dist` contains values not in column `NAME` of `mat.PFG.succ`"))
    # }
    # if (sum(unique(mat.PFG.dist$name) %in% mat.PFG.succ$NAME) < nrow(mat.PFG.succ))
    # {
    #   warning(paste0("Column `name` of `mat.PFG.dist` does not contain all values in column `NAME` of `mat.PFG.succ`"))
    # }
    if (sum(mat.PFG.dist$responseStage %in% seq(1,4)) < nrow(mat.PFG.dist)){
      stop("Wrong type of data!\n Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
    }
    # if (sum(colnames(mat.PFG.dist) %in% paste0("KilledPropagule_", c("H", "C", "P"))) < 3 &&
    #     sum(colnames(mat.PFG.dist) %in% paste0("KilledPropagule_", mat.PFG.succ$NAME)) < nrow(mat.PFG.succ))
    # {
    #   stop(paste0("Wrong type of data!\n Column names of `mat.PFG.dist` must contain either :\n"
    #               , " ==> `KilledPropagule_H`, `KilledPropagule_C`, `KilledPropagule_P`\n"
    #               , " ==> ", paste0("`KilledPropagule_", mat.PFG.succ$NAME, collapse = "`, ")))
    # }
    # if (sum(colnames(mat.PFG.dist) %in% paste0("ActivatedSeed_", c("H", "C", "P"))) < 3 &&
    #     sum(colnames(mat.PFG.dist) %in% paste0("ActivatedSeed_", mat.PFG.succ$NAME)) < nrow(mat.PFG.succ))
    # {
    #   stop(paste0("Wrong type of data!\n Column names of `mat.PFG.dist` must contain either :\n"
    #               , " ==> `ActivatedSeed_H`, `ActivatedSeed_C`, `ActivatedSeed_P`\n"
    #               , " ==> ", paste0("`ActivatedSeed_", mat.PFG.succ$NAME, collapse = "`, ")))
    # }
    if (sum(colnames(mat.PFG.dist) %in% paste0("KilledIndiv_", c("H", "C", "P"))) < 3 &&
        sum(colnames(mat.PFG.dist) %in% paste0("KilledIndiv_", mat.PFG.succ$NAME)) < nrow(mat.PFG.succ))
    {
      stop(paste0("Wrong type of data!\n Column names of `mat.PFG.dist` must contain either :\n"
                  , " ==> `KilledIndiv_H`, `KilledIndiv_C`, `KilledIndiv_P`\n"
                  , " ==> ", paste0("`KilledIndiv_", mat.PFG.succ$NAME, collapse = "`, ")))
    }
    if (sum(colnames(mat.PFG.dist) %in% paste0("ResproutIndiv_", c("H", "C", "P"))) < 3 &&
        sum(colnames(mat.PFG.dist) %in% paste0("ResproutIndiv_", mat.PFG.succ$NAME)) < nrow(mat.PFG.succ))
    {
      stop(paste0("Wrong type of data!\n Column names of `mat.PFG.dist` must contain either :\n"
                  , " ==> `ResproutIndiv_H`, `ResproutIndiv_C`, `ResproutIndiv_P`\n"
                  , " ==> ", paste0("`ResproutIndiv_", mat.PFG.succ$NAME, collapse = "`, ")))
    }
  }

  
  
  
  #################################################################################################
  
  no.PFG = nrow(mat.PFG.succ)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.succ$NAME)
  
  ## GET NUMBER OF DISTURBANCES
  no.DIST = length(unique(as.character(mat.PFG.dist$name)))
  
  ## GET DIST NAME
  DIST_NAME = unique(as.character(mat.PFG.dist$name))
  
  
  cat("\n ############## DIST INFORMATIONS ############## \n")
  cat("\n Number of disturbances : ", no.DIST)
  cat("\n Names of disturbances : ", DIST_NAME)
  cat("\n")
  
  #################################################################################################
  
  ## GET PROPORTION OF KILLED PROPAGULES
  ## 0 for all PFG and disturbances
  # if (sum(colnames(mat.PFG.dist) == paste0("KilledPropagule_", c("H", "C", "P"))) == 3)
  # {
  #   PROP_KILLED = mat.PFG.dist[, paste0("KilledPropagule_", c("H", "C", "P"))]
  # } else if (sum(colnames(mat.PFG.dist) == paste0("KilledPropagule_", mat.PFG.succ$NAME)) == nrow(mat.PFG.succ))
  # {
  #   PROP_KILLED = mat.PFG.dist[, paste0("KilledPropagule_", mat.PFG.succ$NAME)]
  # } else
  # {
  #   PROP_KILLED = matrix(0, nrow = no.DIST, ncol = no.PFG)
  # }
  PROP_KILLED = matrix(0, nrow = no.DIST, ncol = no.PFG)
  
  
  #################################################################################################
  
  ## GET CHANGE between RESPONSE STAGES AGES
  ##   = response classes depend on the age of the PFG
  ## Annuals and biennials won't change their response to disturbances
  no.STAGES = 4
  BREAK_AGE = matrix(0, nrow = no.DIST * (no.STAGES - 1), ncol = no.PFG)
  
  brk_ages_tmp = matrix(0, nrow = no.STAGES - 1, ncol = no.PFG)
  ## A12 = for herbaceous : maturity - 2 / for chamaephyte and phanerophyte : 1
  brk_ages_tmp[1, ] = ifelse(mat.PFG.succ$TYPE == "H", mat.PFG.succ$MATURITY - 2, 1)
  ## A23 = min(CHANG_STR_AGES_to_str_3, maturity)
  brk_ages_tmp[2, ] = apply(cbind(mat.PFG.succ$MATURITY, mat.PFG.succ$CHANG_STR_AGES_to_str_3), 1, min)
  ## A34 = min(CHANG_STR_AGES_to_str_3, longevity - 2)
  brk_ages_tmp[3, ] = apply(cbind(mat.PFG.succ$LONGEVITY - 2, mat.PFG.succ$CHANG_STR_AGES_to_str_3), 1, min)
  
  ## ANNUALS : die after after the first year, so not affected differently according to life stages
  ##   = parameterize only age 1 (from 0 to 1 year)
  ##   = all the rest do not apply as ages 2:4 are never reached  
  brk_ages_tmp[, which(mat.PFG.succ$TYPE == "H" & mat.PFG.succ$LONGEVITY < 2)] = 1
  
  ## BIENNIALS : 
  ##   = parameterize only age 1 for herbaceous
  ##   = parameterize only age 1 and 2 for chamaephyte
  brk_ages_tmp[1, which(mat.PFG.succ$TYPE == "H" & mat.PFG.succ$LONGEVITY == 2)] = 2
  brk_ages_tmp[3, which(mat.PFG.succ$LONGEVITY == 2)] = 2 ## no senescence (never pass to last age class)
  
  ## TRIENNIALS : 
  brk_ages_tmp[1, which(mat.PFG.succ$TYPE == "H" & mat.PFG.succ$LONGEVITY == 3)] = 2
  brk_ages_tmp[3, which(mat.PFG.succ$LONGEVITY == 3)] = 3 ## no senescence (never pass to last age class)
  
  ## SAME FOR ALL DISTURBANCE
  for( i in 1:no.DIST)
  {
    ind_1 = 1 + (i - 1) * (no.STAGES - 1)
    ind_2 = (no.STAGES - 1) + (i - 1) * (no.STAGES - 1)
    BREAK_AGE[ind_1:ind_2, ] = brk_ages_tmp
  }
  
  #################################################################################################
  
  ## GET RESPROUTING AGES
  ##   = living ones are rejuvenated at a younger age
  ##   = does not impact dead individuals
  RESPR_AGE = matrix(0, nrow = no.DIST * no.STAGES, ncol = no.PFG)
  
  ## stage 1 : too young to resprout
  RESPR_AGE[seq(1, nrow(RESPR_AGE), by = no.STAGES), ] = 0
  ## stage 2 : too young to resprout
  RESPR_AGE[seq(2, nrow(RESPR_AGE), by = no.STAGES), ] = 0
  ## stage 3 : juveniles are not affected, matures resprout at maturity - 2
  val.tmp = apply(cbind(mat.PFG.succ$MATURITY - 2, mat.PFG.succ$CHANG_STR_AGES_to_str_3), 1, min)
  RESPR_AGE[seq(3, nrow(RESPR_AGE), by = no.STAGES), ] = rep(val.tmp, each = no.DIST)
  ## stage 4 : resprout at longevity - 2
  RESPR_AGE[seq(4, nrow(RESPR_AGE), by = no.STAGES), ] = rep(mat.PFG.succ$LONGEVITY - 2, each = no.DIST)
  
  ## ANNUALS and BIENNIALS
  ##   = always start back at 0 when resprout, even  in the 3rd age class
  RESPR_AGE[seq(3, nrow(RESPR_AGE), by = no.STAGES), which(mat.PFG.succ$LONGEVITY <= 2)] = 0
  
  
  #################################################################################################
  
  ## GET FATES
  ##   = proportion of killed or resprouting individuals
  ##   = for each disturbance, for each response stage : 2 values
  ##     proportion of killed individuals, and of resprouting individuals
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
  FATES = matrix(0, nrow = no.DIST * no.STAGES * 2, ncol = no.PFG)
  
  for (no.di in 1:no.DIST)
  {
    di = DIST_NAME[no.di]
    ind_dist = which(mat.PFG.dist$name == di) 
    ind_dist = ind_dist[order(mat.PFG.dist$responseStage[ind_dist])]
    
    ## KILLED INDIVIDUALS 
    ind_fates = mat.PFG.dist$responseStage[ind_dist] +
      (mat.PFG.dist$responseStage[ind_dist] - 1) +
      (no.di - 1) * 2 * no.STAGES
    if (sum(colnames(mat.PFG.dist) %in% paste0("KilledIndiv_", c("H", "C", "P"))) == 3)
    {
      FATES[ind_fates, which(mat.PFG.succ$TYPE == "H")] = mat.PFG.dist[ind_dist, "KilledIndiv_H"]
      FATES[ind_fates, which(mat.PFG.succ$TYPE == "C")] = mat.PFG.dist[ind_dist, "KilledIndiv_C"]
      FATES[ind_fates, which(mat.PFG.succ$TYPE == "P")] = mat.PFG.dist[ind_dist, "KilledIndiv_P"]
    } else if (sum(colnames(mat.PFG.dist) == paste0("KilledIndiv_", mat.PFG.succ$NAME)) == nrow(mat.PFG.succ))
    {
      for (pfg in mat.PFG.succ$NAME)
      {
        FATES[ind_fates, which(mat.PFG.succ$NAME == pfg)] = mat.PFG.dist[ind_dist, paste0("KilledIndiv_",pfg)]
      }
    }
    ## RESPROUTING INDIVIDUALS
    ind_fates = ind_fates + 1
    if (sum(colnames(mat.PFG.dist) %in% paste0("ResproutIndiv_", c("H", "C", "P"))) == 3)
    {
      FATES[ind_fates, which(mat.PFG.succ$TYPE == "H")] = mat.PFG.dist[ind_dist, "ResproutIndiv_H"]
      FATES[ind_fates, which(mat.PFG.succ$TYPE == "C")] = mat.PFG.dist[ind_dist, "ResproutIndiv_C"]
      FATES[ind_fates, which(mat.PFG.succ$TYPE == "P")] = mat.PFG.dist[ind_dist, "ResproutIndiv_P"]
    } else if (sum(colnames(mat.PFG.dist) == paste0("ResproutIndiv_", mat.PFG.succ$NAME)) == nrow(mat.PFG.succ))
    {
      for (pfg in mat.PFG.succ$NAME)
      {
        FATES[ind_fates, which(mat.PFG.succ$NAME == pfg)] = mat.PFG.dist[ind_dist, paste0("ResproutIndiv_",pfg)]
      }
    }
  }
  
  #################################################################################################
  ## GET END OF SEED DORMANCY : % of seeds activated by the perturbation
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
  ACTIVATED_SEED = matrix(0, nrow = no.DIST, ncol = no.PFG)
  
  
  #################################################################################################
  
  names.params.list = get("NAME")
  names.params.list.sub = c("NAME"
                            , "PROP_KILLED"
                            , "BREAK_AGE"
                            , "RESPR_AGE"
                            , "FATES"
                            , "ACTIVATED_SEED")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = do.call(rbind, params.list)
  rownames(params.csv) = c("NAME"
                           , paste0("PROP_KILLED_", DIST_NAME)
                           , paste0("BREAK_AGE_", rep(DIST_NAME, each = 3), c("_1to2", "_2to3", "_3to4"))
                           , paste0("RESPR_AGE_", rep(DIST_NAME, each = no.STAGES), "_", 1:no.STAGES)
                           , paste0("FATES_", rep(DIST_NAME, each = no.STAGES * 2), "_"
                                    , paste0(rep(1:no.STAGES, each = 2), c("_kill","_respr")))
                           , paste0("ACTIVATED_SEED_", DIST_NAME))
  
  write.table(params.csv
              , file = paste0(name.simulation, "/DATA/PFGS/DIST_COMPLETE_TABLE.csv")
              , row.names = T
              , col.names = F)
  
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
                                       "/DATA/PFGS/DIST/DIST_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG disturbance parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

