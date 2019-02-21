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
##' @param name.simulation a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.succ a \code{data.frame} with at least 6 columns : \cr
##' PFG, type, MATURITY, LONGEVITY, STRATA and CHANG_STR_AGES_to_str_3. \cr
##' Such an object can be obtained with the \code{PRE_FATE.params_PFGsuccession}
##' function.
##' @param mat.PFG.dist a \code{data.frame} with at least 4 columns : \cr name, 
##' responseStage, KilledIndiv_[...], ResproutIndiv_[...] (see \code{Details})
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} that
##' corresponds to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/DIST/} directory to store the results
##' 
##' 
##' @details
##' 
##' The disturbance module of \code{FATE-HD} allows the user to simulate spatial
##' perturbation(s) that will impact each PFG in terms of resprouting and
##' mortality on the different response stages \emph{(!currently 4, not yet
##' editable!)}. \cr
##' 
##' Several parameters, given within \code{mat.PFG.succ}, are required for each
##' PFG in order to set up these responses :
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
##' These values will allow to calculate or define a set of characteristics for
##' each PFG :
##' 
##' \describe{
##'   \item{BREAK_AGE}{ = each PFG can respond to a disturbance in several 
##'   different ways \cr \emph{(!currently 4, not yet editable!)} that depend 
##'   on the PFG age \cr
##'    = ages at which each PFG changes of response stage \cr \cr
##'    
##'    These response classes are the same for all disturbances for each PFG.
##'    \cr These ages are defined according to the maturity and longevity of the
##'    PFG, its life form and its ability to grow above 1.5 meter :
##'    \itemize{
##'      \item age from class 1 to 2 :
##'      \itemize{
##'        \item \code{maturity - 2} for herbaceous
##'        \item \code{1} for chamaephytes and phanerophytes
##'      }
##'      \item age from class 2 to 3 :
##'      \itemize{
##'        \item \code{maturity} for herbaceous
##'        \item \code{min(maturity - 2 , age_above_150cm)} for chamaephytes 
##'        and phanerophytes
##'      }
##'      \item age from class 3 to 4 :
##'      \itemize{
##'        \item \code{longevity - 2} for herbaceous
##'        \item \code{min(longevity - 2 , age_above_150cm)} for chamaephytes 
##'        and phanerophytes
##'      }
##'    }
##'    Some corrections are made for short-living plants (annuals, biennials...)
##'    : as they die after 1 or 2 years, they are not affected differently 
##'    according to life stages. Break ages from class 1 to 3 are set to 1, and
##'    break age from 3 to 4 is set to their longevity (1 or 2).
##'   }
##'   \item{RESPR_AGE}{ = when subject to a perturbation, each PFG can either
##'   stay undisturbed, be killed, or resprout \emph{(in years)} \cr
##'    = ages at which each PFG will be rejuvenated by a disturbance \cr \cr
##'   It does not impact dead inviduals, only living ones. \cr
##'   It is defined according to the maturity and longevity of the PFG, and its 
##'   ability to grow above 1.5 meter :
##'   \itemize{
##'     \item individuals within 1st and 2nd response stage are too young to 
##'     resprout
##'     \item individuals within 3rd response stage : \cr
##'     \code{min(maturity - 2, age_above_150cm)}
##'     \item individuals within 4th response stage : \code{longevity - 2}
##'     \item short-living plants (annuals, biennials...) always start back 
##'     at 0
##'   }
##'   }
##' }
##' 
##' A second file, \code{mat.PFG.dist}, is required to define the importance of
##' the response of each PFG to each disturbance :
##' 
##' \describe{
##'   \item{name}{the name of each perturbation (several can be defined at the
##'   same time)}
##'   \item{responseStage}{the concerned response class \emph{(!currently 4,
##'   not yet editable!)}}
##'   \item{KilledIndiv_[...]}{the proportion of killed individuals}
##'   \item{ResproutIndiv_[...]}{the proportion of resprouting individuals}
##' }
##' 
##' These values will allow to define a third parameter for each PFG :
##' 
##' \describe{
##'   \item{FATES}{ = proportion of killed or resprouting individuals \cr
##'    = for each disturbance and for each response stage \cr
##'    Two methods are available to give the proportion of killed and
##'    resprouting individuals :
##'    \itemize{
##'      \item for each life form : \code{H} (herbaceous), \code{C}
##'      (chamaephyte) or \code{P} (phanerophyte)
##'      \item for each PFG
##'    }
##'   }
##' }
##' 
##' Two parameters are also defined, but currently set to 0 :
##' 
##' \describe{
##'   \item{PROP_KILLED}{ = the proportion of propagules killed by each 
##'   disturbance \cr \cr
##'   It is currently set to 0 for all PFG and disturbances.
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
##'   \item PROP_KILLED : the proportion of propagules killed by each disturbance \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)}
##'   \item ACTIVATED_SEED : the proportion of seeds activated by each disturbance \cr
##'   \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 8: 80\% 9: 
##'   90\% 10: 100\%)} \cr \cr
##' }
##' 
##' A \code{DIST_COMPLETE_TABLE.csv} file summarizing information for all groups into the
##' \code{name.simulation/DATA/PFGS/} directory.
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the folder
##' \code{name.simulation/DATA/PFGS/DIST/opt.folder.name/}
##' 
##' @keywords FATE, simulation, disturbance, killing, resprouting
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}
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
##'                                                         , longevity = c(12, 200, 25, 4, 110, 70)))
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
  , opt.folder.name = NULL
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/DIST/")
  
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
    .stopMessage_numRowCol("mat.PFG.succ", c("NAME", "TYPE", "MATURITY", "LONGEVITY", "STRATA", "CHANG_STR_AGES_to_str_..."))
  }
  if (ncol(mat.PFG.succ) >= 6)
  {
    if (sum(colnames(mat.PFG.succ) %in% c("NAME", "TYPE", "MATURITY", "LONGEVITY", "STRATA")) < 5 ||
        length(grep("^CHANG_STR_AGES_to_str_", colnames(mat.PFG.succ))) == 0)
    {
      .stopMessage_columnNames("mat.PFG.succ", c("NAME", "TYPE", "MATURITY", "LONGEVITY", "STRATA", "CHANG_STR_AGES_to_str_..."))
    }
  }
  mat.PFG.succ$NAME = as.character(mat.PFG.succ$NAME)
  if (length(which(is.na(mat.PFG.succ$NAME))) > 0 ||
      length(unique(mat.PFG.succ$NAME)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `NAME` of `mat.PFG.succ` must contain different values")
  }
  if (.testParam_notChar(mat.PFG.succ$NAME))
  {
    .stopMessage_beChar("mat.PFG.succ$NAME")
  }
  if (.testParam_notInChar(mat.PFG.succ$TYPE, inList = c("H", "C", "P")))
  {
    .stopMessage_content("mat.PFG.succ$TYPE", c("H", "C", "P"))
  }
  if (!is.numeric(mat.PFG.succ$MATURITY) ||
      !is.numeric(mat.PFG.succ$LONGEVITY) ||
      !is.numeric(mat.PFG.succ$STRATA) ||
      sum(!apply(mat.PFG.succ[, grep("^CHANG_STR_AGES_to_str_", colnames(mat.PFG.succ)), drop = FALSE], 2, is.numeric)) > 0) {
    .stopMessage_columnNumeric("mat.PFG.succ", c("MATURITY", "LONGEVITY", "STRATA", "CHANG_STR_AGES_to_str_..."))
  }
  if (length(which(is.na(mat.PFG.succ$MATURITY))) > 0 ||
      length(which(is.na(mat.PFG.succ$LONGEVITY))) > 0 ||
      length(which(is.na(mat.PFG.succ$STRATA))) > 0 ||
      sum(apply(mat.PFG.succ[, grep("^CHANG_STR_AGES_to_str_", colnames(mat.PFG.succ)), drop = FALSE], 2, is.na)) > 0) {
    .stopMessage_columnNoNA("mat.PFG.succ", c("MATURITY", "LONGEVITY", "STRATA", "CHANG_STR_AGES_to_str_..."))
  }
  
  ## CHECKS for mat.PFG.dist parameter
  if (.testParam_notDf(mat.PFG.dist))
  {
    .stopMessage_beDataframe("mat.PFG.dist")
  }
  if (nrow(mat.PFG.dist) == 0 || ncol(mat.PFG.dist) < 2)
  {
    .stopMessage_numRowCol("mat.PFG.dist", c("name", "responseStage"))
  }
  if (ncol(mat.PFG.dist) >= 1)
  {
    if (sum(colnames(mat.PFG.dist) %in% c("name", "responseStage")) < 2)
    {
      .stopMessage_columnNames("mat.PFG.dist", c("name", "responseStage"))
    }
    mat.PFG.dist$responseStage = as.numeric(as.character(mat.PFG.dist$responseStage))
    
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
  ## CHECKS for parameter opt.folder.name
  if (is.null(opt.folder.name)){
    opt.folder.name = ""
  } else if (!is.null(opt.folder.name) && !is.character(opt.folder.name)){
    warning("As `opt.folder.name` does not contain character value, it will be ignored")
    opt.folder.name = ""
  } else if (nchar(opt.folder.name) > 0){
    opt.folder.name = paste0(opt.folder.name, "/")
    dir.create(paste0(name.simulation, "/DATA/PFGS/DIST/", opt.folder.name))
  } else {
    opt.folder.name = ""
  }

    
  #################################################################################################
  
  mat.PFG.succ$LONGEVITY = mat.PFG.succ$LONGEVITY - 1
  no.PFG = nrow(mat.PFG.succ)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.succ$NAME)
  
  ## GET NUMBER OF DISTURBANCES
  no.DIST = length(unique(as.character(mat.PFG.dist$name)))
  
  ## GET DIST NAME
  DIST_NAME = unique(as.character(mat.PFG.dist$name))
  
  ## GET STRATUM NUMBER whose height >= 150
  names.strata = colnames(mat.PFG.succ)[grep("^CHANG_STR_AGES_to_str_", colnames(mat.PFG.succ))]
  strata.150 = sapply(names.strata, function(x) tail(strsplit(as.character(x), "_")[[1]], 1))
  strata.150 = min(which(strata.150 >= 150))
  strata.150 = names.strata[strata.150]
  
  
  cat("\n ############## DIST INFORMATIONS ############## \n")
  cat("\n Number of disturbances : ", no.DIST)
  cat("\n Names of disturbances : ", DIST_NAME)
  cat("\n")

  
  #################################################################################################
  
  ## GET CHANGE between RESPONSE STAGES AGES
  ##   = response classes depend on the age of the PFG
  ## Annuals and biennials won't change their response to disturbances
  no.STAGES = 4
  BREAK_AGE = matrix(0, nrow = no.DIST * (no.STAGES - 1), ncol = no.PFG)
  
  ind.H = which(mat.PFG.succ$TYPE == "H")
  ind.CP = which(mat.PFG.succ$TYPE != "H")
  
  brk_ages_tmp = matrix(0, nrow = no.STAGES - 1, ncol = no.PFG)
  
  ## A12 = for herbaceous : maturity - 2 / for chamaephyte and phanerophyte : 1
  brk_ages_tmp[1, ] = ifelse(mat.PFG.succ$TYPE == "H", max(mat.PFG.succ$MATURITY - 2, 0), 1)
  
  ## A23 = min(CHANG_STR_AGES_to_str_3, maturity)
  brk_ages_tmp[2, ind.H] = mat.PFG.succ$MATURITY[ind.H]
  brk_ages_tmp[2, ind.CP] = apply(mat.PFG.succ[ind.CP, c("MATURITY", strata.150)], 1, min)
  
  ## A34 = min(CHANG_STR_AGES_to_str_3, longevity - 2)
  brk_ages_tmp[3, ind.H] = mat.PFG.succ$LONGEVITY[ind.H] - 2
  brk_ages_tmp[3, ind.CP] = apply(cbind(mat.PFG.succ$LONGEVITY[ind.CP] - 2
                                        , mat.PFG.succ[ind.CP, strata.150]), 1, min)
  
  ## ANNUALS / BIENNIALS : die after the first or second year, 
  ##   = so not affected differently according to life stages (12, 23 = 1)
  ##   = no senescence (never pass to last age class) (34 = 1 or 2)
  brk_ages_tmp[, which(mat.PFG.succ$LONGEVITY <= 2)] = 1
  brk_ages_tmp[3, which(mat.PFG.succ$LONGEVITY == 2)] = 2 ## no senescence (never pass to last age class)

  ## TRIENNIALS : 
  # brk_ages_tmp[1, which(mat.PFG.succ$TYPE == "H" & mat.PFG.succ$LONGEVITY == 3)] = 2
  # brk_ages_tmp[3, which(mat.PFG.succ$LONGEVITY == 3)] = 3 ## no senescence (never pass to last age class)
  
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
  val.tmp = apply(cbind(max(mat.PFG.succ$MATURITY - 2, 0)
                        , mat.PFG.succ[, strata.150]), 1, min)
  RESPR_AGE[seq(3, nrow(RESPR_AGE), by = no.STAGES), ] = rep(val.tmp, each = no.DIST)
  ## stage 4 : resprout at longevity - 2
  RESPR_AGE[seq(4, nrow(RESPR_AGE), by = no.STAGES), ] = rep(mat.PFG.succ$LONGEVITY - 2, each = no.DIST)
  
  ## ANNUALS and BIENNIALS
  ##   = always start back at 0 when resprout, even in the 3rd age class
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
    } else if (sum(colnames(mat.PFG.dist) %in% paste0("KilledIndiv_", mat.PFG.succ$NAME)) == nrow(mat.PFG.succ))
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
                            , "BREAK_AGE"
                            , "RESPR_AGE"
                            , "FATES"
                            , "PROP_KILLED"
                            , "ACTIVATED_SEED")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = do.call(rbind, params.list)
  rownames(params.csv) = c("NAME"
                           , paste0("BREAK_AGE_", rep(DIST_NAME, each = 3), c("_1to2", "_2to3", "_3to4"))
                           , paste0("RESPR_AGE_", rep(DIST_NAME, each = no.STAGES), "_", 1:no.STAGES)
                           , paste0("FATES_", rep(DIST_NAME, each = no.STAGES * 2), "_"
                                    , paste0(rep(1:no.STAGES, each = 2), c("_kill","_respr")))
                           , paste0("PROP_KILLED_", DIST_NAME)
                           , paste0("ACTIVATED_SEED_", DIST_NAME))
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == "", "", sub("/$", "_", opt.folder.name))
                              , "DIST_COMPLETE_TABLE.csv")
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
    
    .createParams(params.file = paste0(name.simulation
                                       , "/DATA/PFGS/DIST/"
                                       , opt.folder.name
                                       , "DIST_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG disturbance parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

