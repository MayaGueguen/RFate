### HEADER #####################################################################
##' @title Create \emph{LIGHT} parameter files for a \code{FATE-HD} simulation
##' 
##' @name PRE_FATE.params_PFGlight
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' light-related parameters for each PFG (one file for each of them) used in 
##' the light module of \code{FATE-HD}.
##'              
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param mat.PFG.light a \code{data.frame} with 2 to 6 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{type}, (\emph{or \code{active_germ_low}, 
##'   \code{active_germ_medium}, \code{active_germ_high}}) (\emph{or
##'   \code{strategy_ag}})
##'   \item \emph{\code{type}, \code{light_need}}
##' }
##' (see \code{\href{PRE_FATE.params_PFGlight.html#details}{Details}})
##' @param mat.PFG.tol a \code{data.frame} with 2 to 4 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{lifeStage}, \code{resources}, \code{tolerance} 
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \code{\href{PRE_FATE.params_PFGlight.html#details}{Details}})
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} that 
##' corresponds to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/LIGHT/} directory to store the results
##' 
##' 
##' 
##' @details
##' 
##' The light module allows the user to simulate a primary vegetation succession 
##' based on light competition. \cr \cr
##' 
##' Several parameters, given within \code{mat.PFG.light} or \code{mat.PFG.tol}, 
##' are required for each PFG in order to set up the light competition :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. \cr It should be either 
##'   \code{H} (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) 
##'   for now}
##'   \item{(\emph{active_germ_low})}{the proportion of seeds that will 
##'   germinate for \code{Low} light condition}
##'   \item{(\emph{active_germ_medium})}{the proportion of seeds that will 
##'   germinate for \code{Medium} light condition}
##'   \item{(\emph{active_germ_high})}{the proportion of seeds that will 
##'   germinate for \code{High} light condition}
##'   \item{(\emph{strategy_ag})}{a \code{string} to choose the germination 
##'   strategy : \cr \code{light_lover}, \code{indifferent}, \code{shade_lover} 
##'   \cr \cr}
##'   
##'   \item{(\emph{light_need})}{a value between \code{0} and \code{5} 
##'   corresponding to the light preference of the PFG (e.g. from Flora 
##'   Indicativa)\cr \cr}
##'   
##'   \item{lifeStage}{the concerned life stage (\code{Germinant}, 
##'   \code{Immature}, \code{Mature})}
##'   \item{resources}{the concerned light condition (\code{Low}, 
##'   \code{Medium}, \code{High})}
##'   \item{tolerance}{the proportion of surviving individuals}
##'   \item{(\emph{strategy_tol})}{a \code{string} to choose the tolerance 
##'   strategy : \cr \code{full_light}, \code{pioneer}, \code{ubiquist}, 
##'   \code{semi_shade}, \code{undergrowth} \cr \cr}
##' }
##' 
##' 
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' each PFG :
##' 
##' \describe{
##'   \item{ACTIVE_GERM}{proportion of seeds that will germinate for each light 
##'   condition (\code{Low}, \code{Medium}, \code{High}) \cr \cr
##'   Three methods to define these proportions are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} (using \code{strategy_ag}) :
##'     \describe{
##'       \item{}{\strong{\code{| _L_ _M_ _H_ |}}}
##'       \item{}{\code{_______________}}
##'       \item{light_lover}{\code{| 50\% 80\% 90\% |}}
##'       \item{indifferent}{\code{| 90\% 90\% 90\% |}}
##'       \item{shade_lover}{\code{| 90\% 80\% 50\% |}}
##'     }
##'     \item from \strong{predefined rules} (using \code{type}) :
##'     \itemize{
##'       \item for \code{H} (herbaceous) : \code{50\%, 80\%, 90\%}
##'       \item for \code{C} (chamaephyte) or \code{P} (phanerophyte): 
##'       \code{90\%, 90\%, 90\%}
##'     }
##'     \item from \strong{user data} : \cr
##'     \emph{with the values contained within the \code{active_germ_low}, 
##'     \code{active_germ_medium} and \code{active_germ_high} columns, if 
##'     provided \cr \cr}
##'   }
##'   }
##' 
##'   \item{LIGHT_TOL}{ defined for each life stage (\code{Germinant}, 
##'   \code{Immature}, \code{Mature}) \cr and each soil condition (\code{Low}, 
##'   \code{Medium}, \code{High}) \cr \cr
##'   Three methods to define these tolerances are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} (using 
##'     \code{strategy_tol}) : \cr
##'       \itemize{
##'         \item \code{.} means \emph{Not tolerant}, \code{1} means 
##'         \emph{Tolerant} (\code{100\%})
##'         \item with \code{g}: Germinant, \code{i}: Immature, \code{m}: Mature
##'         \item with \code{L}: low light, \code{M}: medium light, \code{H}: 
##'         high light \cr \cr
##'       }
##'     \describe{
##'       \item{}{\strong{\code{| _ g _ | _ i _ | _ m _ |}}}
##'       \item{}{\strong{\code{| L M H | L M H | L M H |}}}
##'       \item{}{\code{_________________________}}
##'       \item{full_light}{\code{| 1 1 1 | . . 1 | . . 1 |}}
##'       \item{pioneer}{\code{| 1 1 1 | . 1 1 | . 1 1 |}}
##'       \item{ubiquist}{\code{| 1 1 1 | 1 1 1 | 1 1 1 |}}
##'       \item{semi_shade}{\code{| 1 1 . | 1 1 . | 1 1 1 |}}
##'       \item{undergrowth}{\code{| 1 1 . | 1 1 . | 1 1 . |}}
##'     }
##'     \item from \strong{predefined rules} (using \code{type} and 
##'     \code{light_need}):
##'       \describe{
##'         \item{(A)}{PFG are tolerant to \code{Low} light if \code{light <= 2}}
##'         \item{(A)}{PFG are tolerant to \code{Medium} light if 
##'         \code{2 <= light <= 4}}
##'         \item{(A)}{PFG are tolerant to \code{High} light if 
##'         \code{light >= 3}}
##'         \item{(B)}{all germinants are assumed to be tolerant to \code{Low} 
##'         light}
##'         \item{(C)}{all mature trees or chamaephytes are assumed to be 
##'         tolerant to \code{Medium} and \code{High} light conditions}
##'         \item{\emph{(D)}}{\emph{all immature trees that grow in the 
##'         penultimate stratum are assumed to be tolerant to \code{High} light} 
##'         \strong{!! desactivated !!}}
##'       }
##'       \itemize{
##'         \item \code{.} means \emph{Not tolerant}
##'         \item \code{A, B, C, D} mean \emph{Tolerant} according to one of 
##'         the rule defined above
##'         \item with \code{g}: Germinant, \code{i}: Immature, \code{m}: Mature
##'         \item with \code{L}: low light, \code{M}: medium light, \code{H}: 
##'         high light \cr \cr
##'       }
##'       \describe{
##'         \item{}{\strong{\code{| _ g _ | _ i _ | _ m _ |}}}
##'         \item{}{\strong{\code{| L M H | L M H | L M H |}}}
##'         \item{}{\code{_________________________}}
##'         \item{1}{\code{| A . . | A . D | A C C |}}
##'         \item{2}{\code{| A A . | A A D | A A C |}}
##'         \item{3}{\code{| B A . | . A D | . A C |}}
##'         \item{4}{\code{| B A A | . A A | . A A |}}
##'         \item{5}{\code{| B . A | . . A | . C A |}}
##'       }
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{lifeStage}, 
##'       \code{resources} and \code{tolerance} columns, if provided}
##'   }
##'   }
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/LIGHT/} directory with the following 
##' parameters :
##' 
##' \describe{
##'   \item{NAME}{name of the PFG}
##'   \item{ACTIVE_GERM}{the germination rates depending on light conditions 
##'   \cr \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}}
##'   \item{LIGHT}{light value or strategy of the PFG}
##'   \item{LIGHT_TOL}{the PFG light tolerance table (in a single row). \cr 
##'   This is a vector of 9 numbers corresponding to the ability of the PFG to 
##'   survive or not :
##'   \itemize{
##'     \item at different life stages \emph{(Germinant (Ge), Immature (Im), 
##'     Mature (Ma))}
##'     \item under different light conditions \emph{(Low (L), Medium (M) or 
##'     High (H))}.
##'   }
##'   These parameters should be given in this order : GeL, GeM, GeH, ImL, ImM, 
##'   ImH, MaL, MaM, MaH.
##'   \cr \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}}
##' }
##' 
##' A \code{LIGHT_COMPLETE_TABLE.csv} file summarizing information for all 
##' groups into the \code{name.simulation/DATA/PFGS/} directory.  
##'
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \code{name.simulation/DATA/PFGS/LIGHT/opt.folder.name/}
##' 
##' 
##' 
##' @keywords FATE, simulation, light tolerance
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG light parameter files
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.light = data.frame(PFG = paste0("PFG",1:6)
##'                                                      , type = c("C", "C", "H", "H", "P", "P")
##'                                                      , light_need = c(4, 6, 3, 6, 5, 5)))
##' 
##' 
##' ## Create PFG light parameter files
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.light = data.frame(PFG = paste0("PFG",1:6)
##'                                                      , type = c("C", "C", "H", "H", "P", "P"))
##'                          , mat.PFG.tol = data.frame(PFG = paste0("PFG",1:6)
##'                                                     , light = c("ubiquist", "ubiquist"
##'                                                                 , "semi_shade", "ubiquist"
##'                                                                 , "full_light", "pioneer")))
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##' 
##' ## Load example data
##' PNE_PARAM = .loadData("PNE_PARAM")
##' 
##' ## PNE_PARAM$succ_light : data.frame
##' 
##' tab = PNE_PARAM$succ_light[, c("PFG", "type", "light")]
##' colnames(tab)[3] = "light_need"
##' 
##' ## Create PFG light parameter files : 
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.light = tab)
##'                          
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGlight = function(
  name.simulation
  , mat.PFG.light
  , mat.PFG.tol = NULL
  , opt.folder.name = NULL
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/LIGHT/")
  
  ## CHECKS for parameter mat.PFG.light
  if (.testParam_notDf(mat.PFG.light))
  {
    .stopMessage_beDataframe("mat.PFG.light")
  } else
  {
    if (nrow(mat.PFG.light) == 0 || ncol(mat.PFG.light) < 2 || ncol(mat.PFG.light) > 6)
    {
      .stopMessage_numRowCol("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
    } else if (ncol(mat.PFG.light) == 2)
    {
      if ((sum(colnames(mat.PFG.light) == c("PFG", "type")) < 2) &&
          (sum(colnames(mat.PFG.light) == c("PFG", "strategy_ag")) < 2))
      {
        .stopMessage_columnNames("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                    , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
      }
    } else if (ncol(mat.PFG.light) == 3)
    {
      if (sum(colnames(mat.PFG.light) %in% c("PFG", "type", "light_need")) < 3)
      {
        .stopMessage_columnNames("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                    , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
      }
    } else if (ncol(mat.PFG.light) == 4)
    {
      if ((sum(colnames(mat.PFG.light) %in% c("PFG", "active_germ_low", "active_germ_medium", "active_germ_high")) < 4) &&
          (sum(colnames(mat.PFG.light) %in% c("PFG", "strategy_ag", "type", "light_need")) < 4))
      {
        .stopMessage_columnNames("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                    , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
      }
    } else if (ncol(mat.PFG.light) == 6)
    {
      if (sum(colnames(mat.PFG.light) %in% c("PFG", "active_germ_low", "active_germ_medium", "active_germ_high", "type", "light_need")) < 4)
      {
        .stopMessage_columnNames("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                    , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
      }
    } else
    {
      .stopMessage_columnNames("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                  , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
    }
    
    
    mat.PFG.light$PFG = as.character(mat.PFG.light$PFG)
    mat.PFG.light$type = as.character(mat.PFG.light$type)
    if (length(which(is.na(mat.PFG.light$PFG))) > 0 ||
        length(unique(mat.PFG.light$PFG)) < nrow(mat.PFG.light)){
      stop("Wrong type of data!\n Column `PFG` of `mat.PFG.light` must contain different values")
    }
    if (.testParam_notChar(mat.PFG.light$PFG))
    {
      .stopMessage_beChar("mat.PFG.light$PFG")
    }
    ## CHECKS for type column
    if (sum(colnames(mat.PFG.light) == "type") == 1)
    {
      if (.testParam_notInChar(mat.PFG.light$type, inList = c("H", "C", "P")))
      {
        .stopMessage_content("mat.PFG.light$type", c("H", "C", "P"))
      }
    }
    ## CHECKS for light_need column
    if (sum(colnames(mat.PFG.light) == "light_need") == 1)
    {
      if (!is.numeric(mat.PFG.light$light_need_need)) {
        .stopMessage_columnNumeric("mat.PFG.light", c("light_need"))
      }
      if (length(which(is.na(mat.PFG.light$light_need_need))) > 0) {
        .stopMessage_columnNoNA("mat.PFG.light", c("light_need"))
      }
      if (sum(mat.PFG.light$light_need_need %in% seq(0,5)) < nrow(mat.PFG.light)){
        .stopMessage_columnBetween("mat.PFG.light", "light_need", 0, 5)
      }
    }
    ## CHECKS for active_germ_... columns
    if (sum(colnames(mat.PFG.light) == "active_germ_low") == 1)
    {
      if (!is.numeric(mat.PFG.light$active_germ_low) ||
          !is.numeric(mat.PFG.light$active_germ_medium) ||
          !is.numeric(mat.PFG.light$active_germ_high)) {
        .stopMessage_columnNumeric("mat.PFG.light", c("active_germ_low", "active_germ_medium", "active_germ_high"))
      }
      if (length(which(is.na(mat.PFG.light$active_germ_low))) > 0 ||
          length(which(is.na(mat.PFG.light$active_germ_medium))) > 0 ||
          length(which(is.na(mat.PFG.light$active_germ_high))) > 0) {
        .stopMessage_columnNoNA("mat.PFG.light", c("active_germ_low", "active_germ_medium", "active_germ_high"))
      }
      if (sum(mat.PFG.light$active_germ_low %in% seq(0,10)) < nrow(mat.PFG.light) ||
          sum(mat.PFG.light$active_germ_medium %in% seq(0,10)) < nrow(mat.PFG.light) ||
          sum(mat.PFG.light$active_germ_high %in% seq(0,10)) < nrow(mat.PFG.light)) {
        .stopMessage_columnBetween("mat.PFG.light", c("active_germ_low", "active_germ_medium", "active_germ_high"), 0, 10)
      }
    }
    ## CHECKS for strategy_ag column
    if (sum(colnames(mat.PFG.light) == "strategy_ag") == 1)
    {
      mat.PFG.light$strategy_ag = as.character(mat.PFG.light$strategy_ag)
      if (.testParam_notInChar(mat.PFG.light$strategy_ag, inList = c("light_lover", "indifferent", "shade_lover")))
      {
        .stopMessage_content("mat.PFG.light$strategy_ag", c("light_lover", "indifferent", "shade_lover"))
      }
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
      if (nrow(mat.PFG.tol) == 0 || (ncol(mat.PFG.tol) != 2 || ncol(mat.PFG.tol) != 4))
      {
        .stopMessage_numRowCol("mat.PFG.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
      } else if (ncol(mat.PFG.tol) == 4)
      {
        if (sum(colnames(mat.PFG.tol) == c("PFG", "lifeStage", "resources", "tolerance")) < 4)
        {
          .stopMessage_columnNames("mat.PFG.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
        }
      } else if (ncol(mat.PFG.tol) == 2)
      {
        if (sum(colnames(mat.PFG.tol) == c("PFG", "strategy_tol")) < 2)
        {
          .stopMessage_columnNames("mat.PFG.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
        }
      }
      mat.PFG.tol$PFG = as.character(mat.PFG.tol$PFG)
      if (.testParam_notChar(mat.PFG.tol$PFG))
      {
        .stopMessage_beChar("mat.PFG.tol$PFG")
      }
      ## CHECKS for user data option
      if (sum(colnames(mat.PFG.tol) == "lifeStage") == 1)
      {
        if (.testParam_notInChar(mat.PFG.tol$lifeStage, inList = c("Germinant", "Immature", "Mature")))
        {
          .stopMessage_content("mat.PFG.tol$lifeStage", c("Germinant", "Immature", "Mature"))
        }
        if (.testParam_notInChar(mat.PFG.tol$resources, inList = c("Low", "Medium", "High")))
        {
          .stopMessage_content("mat.PFG.tol$resources", c("Low", "Medium", "High"))
        }
        if (!is.numeric(mat.PFG.tol$tolerance)) {
          .stopMessage_columnNumeric("mat.PFG.tol", c("tolerance"))
        }
        if (length(which(is.na(mat.PFG.tol$tolerance))) > 0) {
          .stopMessage_columnNoNA("mat.PFG.tol", c("tolerance"))
        }
        if (sum(mat.PFG.tol$tolerance %in% seq(0,10)) < nrow(mat.PFG.tol)){
          .stopMessage_columnBetween("mat.PFG.tol", "tolerance", 0, 10)
        }
      }
      ## CHECKS for strategy_tol column
      if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
      {
        mat.PFG.tol$strategy_tol = as.character(mat.PFG.tol$strategy_tol)
        if (.testParam_notInChar(mat.PFG.tol$strategy_tol, inList = c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth")))
        {
          .stopMessage_content("mat.PFG.tol$strategy_tol", c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
        }
      }
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
    dir.create(paste0(name.simulation, "/DATA/PFGS/LIGHT/", opt.folder.name))
  } else {
    opt.folder.name = ""
  }
  
  #################################################################################################
  
  no.PFG = nrow(mat.PFG.light)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.light$PFG)
  
  ## GET PFG LIGHT
  LIGHT = rep("", no.PFG)
  if (sum(colnames(mat.PFG.light) == "light_need") == 1)
  {
    LIGHT = as.character(mat.PFG.light$light_need)
  } else if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
  {
    LIGHT = as.character(mat.PFG.tol$strategy_tol)
  } else
  {
    warning("Missing data! The `LIGHT` parameter has not been set. Please check.")
  }
  
  #################################################################################################
  
  ## GET GERMINATION RATE depending on light conditions
  ##   = these rates should express a deviation from the
  ##     germination rate in optimal conditions (=100%)
  ##   = for each light condition (Low, Medium, High)
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
  
  if (sum(colnames(mat.PFG.light) == "type") == 1)
  {
    ## woody species have little variation in germination rate depending on light conditions
    ACTIVE_GERM[, which(mat.PFG.light$type %in% c("C", "P"))] = 9
    ## herbaceous germinate less in the shadow
    ACTIVE_GERM[1, which(mat.PFG.light$type == "H")] = 5 ## low light conditions
    ACTIVE_GERM[2, which(mat.PFG.light$type == "H")] = 8 ## medium light conditions
    ACTIVE_GERM[3, which(mat.PFG.light$type == "H")] = 9 ## high light conditions
    
  } else if (sum(colnames(mat.PFG.light) == "active_germ_low") == 1 ||
             sum(colnames(mat.PFG.light) == "active_germ_medium") == 1 ||
             sum(colnames(mat.PFG.light) == "active_germ_high") == 1)
  {
    if (sum(colnames(mat.PFG.light) == "active_germ_low") == 1)
    {
      ACTIVE_GERM[1, ] = mat.PFG.light$active_germ_low
    }
    if (sum(colnames(mat.PFG.light) == "active_germ_medium") == 1)
    {
      ACTIVE_GERM[2, ] = mat.PFG.light$active_germ_medium
    }
    if (sum(colnames(mat.PFG.light) == "active_germ_high") == 1)
    {
      ACTIVE_GERM[3, ] = mat.PFG.light$active_germ_high
    }
    
  } else if (sum(colnames(mat.PFG.light) == "strategy_ag") == 1)
  {
    for (i in 1:no.PFG){
      ACTIVE_GERM[, i] = switch(mat.PFG.light$strategy_ag[i]
                                , light_lover = c(5, 8, 9)
                                , indifferent = c(9, 9, 9)
                                , shade_lover = c(9, 8, 5)
      )
    }
  } else
  {
    warning("Missing data! The `ACTIVE_GERM` parameter has not been set. Please check.")
  }

  #################################################################################################
  
  ## GET SHADE TOLERANCE
  ##    = for each life stage (Germinant, Immature, Mature)
  ##    = and for each light condition (Low, Medium, High)
  ## 0 = non tolerant
  ## 1 = tolerant
  
  ## FLORA INDICATIVA
  ##   1 = deep shade (tolerate light < 3 %)
  ##   2 = shade (rarely tolerate light < 3%, often light < 10%)
  ##   3 = semi-shade (rarely tolerate light < 10%)
  ##   4 = well lit places (tolerate low shade only occasionally)
  ##   5 = full light (only open and sunny)
  
  ## ELLENBERG
  ##   1 = deep shade
  ##   3 = shade (mostly tolerate light < 5%, often light < 30%, more for grown trees)
  ##   5 = semi-shade (rarely tolerate light < 10%)
  ##   7 = well lit places (tolerate partial shade occasionally)
  ##   9 = full light (only open and sunny)
  
  LIGHT_TOL = matrix(0, nrow = 3 * 3, ncol = no.PFG)
  
  if (is.null(mat.PFG.tol))
  {
    if (sum(colnames(mat.PFG.light) == "type") == 1 &&
        sum(colnames(mat.PFG.light) == "light_need") == 1)
    {
    for (i in 1:no.PFG){
      ## Low light condition
      if (mat.PFG.light$light_need[i] <= 2)
      {
        LIGHT_TOL[c(1, 4, 7), i] = 1
      }
      ## Medium light condition
      if (mat.PFG.light$light_need[i] >= 2 && mat.PFG.light$light_need[i] <= 4)
      {
        LIGHT_TOL[c(2, 5, 8), i] = 1
      }
      ## High light condition
      if (mat.PFG.light$light_need[i] >= 3)
      {
        LIGHT_TOL[c(3, 6, 9), i] = 1
      }
    }
    
    ## All germinants are assumed to be tolerant to Low light
    LIGHT_TOL[c(1),] = 1
    ## All mature trees and shrubs are assumed to be tolerant to Low and Medium Light
    LIGHT_TOL[c(8, 9), which(mat.PFG.light$type %in% c("C", "P"))] = 1
    ## All immature trees that grow in the penultimate stratum are assumed to be tolerant to High light
    # LIGHT_TOL[c(6), which(mat.PFG.light$type == "P" & CHANG_STR_AGES[nrow(CHANG_STR_AGES) - 1,] < MATURITY)] = 1
    
    ## What about all germinant tolerant to Medium light ?
    ## What about all mature trees and shrubs tolerant to Low light ?
    
    } else
    {
      warning("Missing data! The `LIGHT_TOL` parameter has not been set. Please check.")
    }
  } else
  {
    if (sum(colnames(mat.PFG.tol) == "lifeStage") == 1)
    {
      for (ii in 1:nrow(mat.PFG.tol))
      {
        LS_res = paste0(mat.PFG.tol$lifeStage[ii], "_", mat.PFG.tol$resources[ii])
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
        LIGHT_TOL[ind_ii, which(NAME == mat.PFG.tol$PFG[ii])] = mat.PFG.tol$tolerance[ii]
      }
    } else if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
    {
      for (i in 1:no.PFG){
        LIGHT_TOL[, i] = switch(mat.PFG.light$light_need[i]
                                , full_light = c(1,1,1,0,0,1,0,0,1)
                                , pioneer = c(1,1,1,0,1,1,0,1,1)
                                , ubiquist = c(1,1,1,1,1,1,1,1,1)
                                , semi_shade = c(1,1,0,1,1,0,1,1,1)
                                , undergrowth = c(1,1,0,1,1,0,1,1,0)
        )
      }
    } else
    {
      warning("Missing data! The `LIGHT_TOL` parameter has not been set. Please check.")
    }
  }
  
  
  #################################################################################################
  
  names.params.list = get("NAME")
  names.params.list.sub = c("NAME"
                            , "LIGHT"
                            , "ACTIVE_GERM"
                            , "LIGHT_TOL")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "LIGHT"
                           , paste0("ACTIVE_GERM_for_", c("L", "M", "H"))
                           , paste0("LIGHT_TOL_for_",
                                    c("GeL", "GeM", "GeH", "ImL", "ImM", "ImH", "MaL", "MaM", "MaH")))
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == "", "", sub("/$", "_", opt.folder.name))
                              , "LIGHT_COMPLETE_TABLE.csv")
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
    
    .createParams(params.file = paste0(name.simulation
                                       , "/DATA/PFGS/LIGHT/"
                                       , opt.folder.name
                                       , "LIGHT_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG light parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

