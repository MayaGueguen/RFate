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
##' @param mat.PFG.succ a \code{data.frame} with at least 6 columns : \cr 
##' \code{PFG}, \code{type}, \code{height}, \code{maturity}, \code{longevity}, 
##' \code{light} \cr (and optionally, \code{immature_size}, 
##' \code{active_germ_low}, \code{active_germ_medium}, \code{active_germ_high}) 
##' \cr (see \code{\href{PRE_FATE.params_PFGlight.html#details}{Details}})
##' @param mat.PFG.tol (\emph{optional}) \cr a \code{data.frame} with 4 columns 
##' : \code{PFG}, \code{lifeStage}, \code{lightResources}, \code{light_tol}
##' @param strata.limits a \code{vector} of \code{integer} containing values 
##' among which height strata limits will be chosen
##' @param strata.limits_reduce default \code{TRUE}. If \code{TRUE}, stratum 
##' height limits are checked to try and bring several PFGs together in a same 
##' stratum.
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} that 
##' corresponds to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/LIGHT/} directory to store the results
##' 
##' 
##' 
##' @details
##' 
##' The light module allows the user to simulate a primary vegetation 
##' succession based on light competition. \cr
##' Several parameters are required for each PFG in order to set up this life 
##' cycle :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. It should be either \code{H} 
##'   (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) for now}
##'   \item{height}{the maximum or average height that reach the PFG}
##'   \item{maturity}{the age from which the PFG can reproduce}
##'   \item{longevity}{the maximum or average lifespan of the PFG}
##'   \item{light}{two options :
##'     \itemize{
##'       \item a value between \code{0} and \code{5} corresponding to the 
##'       light preference of the PFG (e.g. from Flora Indicativa)
##'       \item a \code{string} to choose the light strategy : \cr
##'       \code{full_light}, \code{pioneer}, \code{ubiquist}, 
##'       \code{semi_shade}, \code{undergrowth}
##'     }
##'   }
##'   \item{(\emph{immature_size})}{the relative size of immature versus mature 
##'   plants}
##'   \item{(\emph{active_germ_low})}{the proportion of seeds that will 
##'   germinate for \code{Low} light condition}
##'   \item{(\emph{active_germ_medium})}{the proportion of seeds that will 
##'   germinate for \code{Medium} light condition}
##'   \item{(\emph{active_germ_high})}{the proportion of seeds that will 
##'   germinate for \code{High} light condition \cr \cr}
##' }
##' 
##' 
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' each PFG :
##' 
##' \describe{
##'   \item{STRATA_LIMITS}{(see \code{\link{PRE_FATE.params_PFGsuccession}})}
##'   \item{STRATA}{(see \code{\link{PRE_FATE.params_PFGsuccession}})}
##'   \item{MAX_ABUNDANCE}{(see \code{\link{PRE_FATE.params_PFGsuccession}})}
##'   \item{IMM_SIZE}{(see \code{\link{PRE_FATE.params_PFGsuccession}})}
##'   \item{CHANG_STR_AGES}{(see \code{\link{PRE_FATE.params_PFGsuccession}}) 
##'   \cr \cr}
##'   \item{ACTIVE_GERM}{proportion of seeds that will germinate for each light 
##'   condition (\code{Low}, \code{Medium}, \code{High}) :
##'   \itemize{
##'     \item for \code{H} (herbaceous) : \code{50\%, 80\%, 90\%}
##'     \item for \code{C} (chamaephyte) : \code{90\%, 90\%, 90\%}
##'     \item for \code{P} (phanerophyte) : \code{90\%, 90\%, 90\%}
##'   }
##'   \emph{or the values contained within the \code{active_germ_low}, 
##'   \code{active_germ_medium} and \code{active_germ_high} columns, if provided 
##'   \cr \cr}
##'   }
##'   \item{SHADE_TOL}{ defined for each life stage (Germinant, Immature, 
##'   Mature) and each light condition (\code{Low}, \code{Medium}, 
##'   \code{High}) \cr \cr
##'   Two methods to define these tolerances are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} : \cr
##'       \itemize{
##'         \item \code{.} means \emph{Not tolerant}, \code{1} means 
##'         \emph{Tolerant}
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
##'     \item from \strong{specific data and rules} :
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
##'         \item{(D)}{all immature trees that grow in the penultimate stratum 
##'         are assumed to be tolerant to \code{High} light}
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
##'   \item{LIGHT}{light value or strategy of the PFG}
##'   \item{ACTIVE_GERM}{the germination rates depending on light conditions 
##'   \cr \emph{(0: 0\% 1: 10\% 2: 20\% 3: 30\% 4: 40\% 5: 50\% 6: 60\% 7: 70\% 
##'   8: 80\% 9: 90\% 10: 100\%)}}
##'   \item{SHADE_TOL}{the PFG shade tolerance table (in a single row). \cr 
##'   This is a vector of 9 numbers \emph{(0: Die 1: Survive)} corresponding 
##'   to the ability of the PFG to survive or not :
##'   \itemize{
##'     \item at different life stages \emph{(Germinant (Ge), Immature (Im), 
##'     Mature (Ma))}
##'     \item under different light conditions \emph{(Low (L), Medium (M) or 
##'     High (H))}.
##'   }
##'   These parameters should be given in this order : GeL, GeM, GeH, ImL, ImM, 
##'   ImH, MaL, MaM, MaH.}
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
##' @keywords FATE, simulation, light, shade tolerance
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
##'                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                      , type = c("C", "C", "H", "H", "P", "P")
##'                                                      , height = c(10, 250, 36, 68, 1250, 550)
##'                                                      , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                      , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                      , light = c(4, 6, 3, 6, 5, 5)))
##'                                                      
##' ## Create PFG light parameter files
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                      , type = c("C", "C", "H", "H", "P", "P")
##'                                                      , height = c(10, 250, 36, 68, 1250, 550)
##'                                                      , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                      , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                      , light = c(4, 6, 3, 6, 5, 5)
##'                                                      , immature_size = c(10, 8, 10, 10, 1, 5)))
##' 
##' 
##' ## Create PFG light parameter files
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                      , type = c("C", "C", "H", "H", "P", "P")
##'                                                      , height = c(10, 250, 36, 68, 1250, 550)
##'                                                      , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                      , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                      , light = c("ubiquist", "ubiquist"
##'                                                                  , "semi_shade", "ubiquist"
##'                                                                  , "full_light", "pioneer")))
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##' 
##' ## Load example data
##' PNE_PARAM = .loadData("PNE_PARAM")
##' 
##' ## PNE_PARAM$succ_light : data.frame
##' ## PNE_PARAM$strata_limits : vector
##' 
##' tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity", "light")]
##' 
##' ## Create PFG light parameter files : automatic definition of strata limits
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.succ = tab)
##'                             
##' ## Create PFG light parameter files : predefined of strata limits
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
##'                          , mat.PFG.succ = tab
##'                          , strata.limits = PNE_PARAM$strata_limits
##'                          , strata.limits_reduce = FALSE)
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGlight = function(
  name.simulation
  , mat.PFG.succ
  , strata.limits = c(0, 20, 50, 150, 400, 1000, 2000, 5000, 10000)
  , strata.limits_reduce = TRUE
  , opt.folder.name = NULL
){
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/LIGHT/")
  
  if (.testParam_notDf(mat.PFG.succ))
  {
    .stopMessage_beDataframe("mat.PFG.succ")
  }
  if (nrow(mat.PFG.succ) == 0 || ncol(mat.PFG.succ) < 6)
  {
    .stopMessage_numRowCol("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity", "light"))
  } else if (ncol(mat.PFG.succ) == 6)
  {
    if (sum(colnames(mat.PFG.succ) == c("PFG", "type","height", "maturity", "longevity", "light")) < 6)
    {
      .stopMessage_columnNames("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity", "light"))
    }
  } else if (ncol(mat.PFG.succ) > 6)
  {
    if (!((sum(colnames(mat.PFG.succ) %in% c("PFG", "type","height", "maturity", "longevity", "light")) == 6) &&
          (sum(colnames(mat.PFG.succ) %in% c("immature_size", "active_germ_low", "active_germ_medium", "active_germ_high")) == (ncol(mat.PFG.succ) - 6))))
    {
      .stopMessage_columnNames("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity", "light"
                                                 , "(immature_size)", "(active_germ_low)", "(active_germ_medium)", "(active_germ_high)"))
    }
  }
  
  mat.PFG.succ$PFG = as.character(mat.PFG.succ$PFG)
  mat.PFG.succ$type = as.character(mat.PFG.succ$type)
  if (length(which(is.na(mat.PFG.succ$PFG))) > 0 ||
      length(unique(mat.PFG.succ$PFG)) < nrow(mat.PFG.succ)){
    stop("Wrong type of data!\n Column `PFG` of `mat.PFG.succ` must contain different values")
  }
  if (.testParam_notChar(mat.PFG.succ$PFG))
  {
    .stopMessage_beChar("mat.PFG.succ$PFG")
  }
  if (.testParam_notInChar(mat.PFG.succ$type, inList = c("H", "C", "P")))
  {
    .stopMessage_content("mat.PFG.succ$type", c("H", "C", "P"))
  }
  if (!is.numeric(mat.PFG.succ$height) ||
      !is.numeric(mat.PFG.succ$maturity) ||
      !is.numeric(mat.PFG.succ$longevity))
  {
    .stopMessage_columnNumeric("mat.PFG.succ", c("height", "maturity", "longevity"))
  }
  if (length(which(is.na(mat.PFG.succ$height))) > 0 ||
      length(which(is.na(mat.PFG.succ$maturity))) > 0 ||
      length(which(is.na(mat.PFG.succ$longevity))) > 0 ||
      length(which(is.na(mat.PFG.succ$light))) > 0)
  {
    .stopMessage_columnNoNA("mat.PFG.succ", c("height", "maturity", "longevity", "light"))
  }
  doLightStrategy = FALSE
  if(!is.numeric(mat.PFG.succ$light))
  {
    mat.PFG.succ$light = as.character(mat.PFG.succ$light)
    if (.testParam_notInChar(mat.PFG.succ$light, inList = c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth")))
    {
      .stopMessage_content("mat.PFG.succ$light", c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
    } else
    {
      doLightStrategy = TRUE
    }
  } else
  {
    if (sum(mat.PFG.succ$light %in% seq(0,10)) < nrow(mat.PFG.succ)){
      stop("Wrong type of data!\n Column `light` of `mat.PFG.succ` must contain values between 0 and 10")
    }
  }
  if (sum(colnames(mat.PFG.succ) == "immature_size") == 1)
  {
    if (!is.numeric(mat.PFG.succ$immature_size))
    {
      .stopMessage_columnNumeric("mat.PFG.succ", "immature_size")
    }
    if (length(which(is.na(mat.PFG.succ$immature_size))) > 0)
    {
      .stopMessage_columnNoNA("mat.PFG.succ", "immature_size")
    }
    if (sum(mat.PFG.succ$immature_size %in% seq(0,10)) < nrow(mat.PFG.succ))
    {
      stop("Wrong type of data!\n Column `immature_size` of `mat.PFG.succ` must contain values between 0 and 10")
    }
  }
  if (sum(colnames(mat.PFG.succ) == "active_germ_low") == 1)
  {
    if (!is.numeric(mat.PFG.succ$active_germ_low))
    {
      .stopMessage_columnNumeric("mat.PFG.succ", "active_germ_low")
    }
    if (length(which(is.na(mat.PFG.succ$active_germ_low))) > 0)
    {
      .stopMessage_columnNoNA("mat.PFG.succ", "active_germ_low")
    }
    if (sum(mat.PFG.succ$active_germ_low %in% seq(0,10)) < nrow(mat.PFG.succ))
    {
      stop("Wrong type of data!\n Column `active_germ_low` of `mat.PFG.succ` must contain values between 0 and 10")
    }
  }
  if (sum(colnames(mat.PFG.succ) == "active_germ_medium") == 1)
  {
    if (!is.numeric(mat.PFG.succ$active_germ_medium))
    {
      .stopMessage_columnNumeric("mat.PFG.succ", "active_germ_medium")
    }
    if (length(which(is.na(mat.PFG.succ$active_germ_medium))) > 0)
    {
      .stopMessage_columnNoNA("mat.PFG.succ", "active_germ_medium")
    }
    if (sum(mat.PFG.succ$active_germ_medium %in% seq(0,10)) < nrow(mat.PFG.succ))
    {
      stop("Wrong type of data!\n Column `active_germ_medium` of `mat.PFG.succ` must contain values between 0 and 10")
    }
  }
  if (sum(colnames(mat.PFG.succ) == "active_germ_high") == 1)
  {
    if (!is.numeric(mat.PFG.succ$active_germ_high))
    {
      .stopMessage_columnNumeric("mat.PFG.succ", "active_germ_high")
    }
    if (length(which(is.na(mat.PFG.succ$active_germ_high))) > 0)
    {
      .stopMessage_columnNoNA("mat.PFG.succ", "active_germ_high")
    }
    if (sum(mat.PFG.succ$active_germ_high %in% seq(0,10)) < nrow(mat.PFG.succ))
    {
      stop("Wrong type of data!\n Column `active_germ_high` of `mat.PFG.succ` must contain values between 0 and 10")
    }
  }
  strata.limits = sort(unique(na.exclude(strata.limits)))
  if (.testParam_notNum(strata.limits) ||
      length(which(strata.limits < 0)) > 0)
  {
    .stopMessage_beInteger("strata.limits")
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
  
  no.PFG = nrow(mat.PFG.succ)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.succ$PFG)
  
  ## GET LIGHT
  LIGHT = mat.PFG.succ$light
  
  ## GET PFG TYPE
  TYPE = as.character(mat.PFG.succ$type)

  ## GET MATURITY AGE values
  MATURITY = mat.PFG.succ$maturity
  
  ## GET LONGEVITY values
  ## Death precedes seed productivity in the model thus longevity param = longevity + 1
  LONGEVITY = mat.PFG.succ$longevity + 1
  
  cat("\n ############## GROUP INFORMATIONS ############## \n")
  cat("\n Number of groups : ", no.PFG)
  cat("\n Number of PFG of each type : "
      , length(which(TYPE == "H")), " H, "
      , length(which(TYPE == "C")), " C, "
      , length(which(TYPE == "P")), " P, ")
  cat("\n")
  
  #################################################################################################
  
  ## GET height strata limits (for light competition and PFG growth)
  ## n strata (+ germinants = 0)
  if (strata.limits_reduce)
  {
    no.PFG.perStrata = round(sqrt(no.PFG))
    categories = cut(mat.PFG.succ$height, breaks = strata.limits)
    categories.table = table(categories)
    if (no.PFG == 1)
    {
      categ = which(categories.table == 1)
      STRATA_LIMITS = c(0, strata.limits[categ], strata.limits[categ + 1])    
    } else
    {
      STRATA_LIMITS = 0
      tmp = categories.table[1]
      for (categ in 2:length(strata.limits))
      {
        if (tmp >= max(c(1, (no.PFG.perStrata - 2))))
        {
          STRATA_LIMITS = c(STRATA_LIMITS, strata.limits[categ])
          tmp = categories.table[categ]
        } else 
        {
          tmp = tmp + categories.table[categ]
        }
      }
    }
    STRATA_LIMITS = sort(unique(STRATA_LIMITS))
  } else
  {
    STRATA_LIMITS = strata.limits
  }
  # barplot(table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  
  ## GET STRATA attribution
  STRATA = sapply(mat.PFG.succ$height, function(h) {
    max(which(STRATA_LIMITS < h), na.rm = T)
  })
  
  no.strata = max(STRATA)
  
  cat("\n ############## STRATA INFORMATIONS ############## \n")
  cat("\n Number of strata : ", no.strata)
  cat("\n Height limits of selected strata : ", STRATA_LIMITS)
  cat("\n Number of PFG within each stratum : ", table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  cat("\n")
  
  #################################################################################################
  
  ## GET IMMATURE SIZE
  ##   = relative shade of immature plants
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
  if (sum(colnames(mat.PFG.succ) == "immature_size") == 1)
  {
    IMM_SIZE = mat.PFG.succ$immature_size
  } else
  {
    IMM_SIZE = rep(10, no.PFG)
    IMM_SIZE[which(mat.PFG.succ$type == "H")] = 10 ## immature herbaceous contribute to shade in the same way than mature herbaceous
    IMM_SIZE[which(mat.PFG.succ$type == "C")] = 5 ## immature chamaephytes contribute to shade half less than mature herbaceous
    IMM_SIZE[which(mat.PFG.succ$type == "P")] = 1 ## immature phanerophytes contribute to shade only by 10 % of their full capacity
    IMM_SIZE[which(mat.PFG.succ$type == "H" & STRATA == 2)] = 8 ## intermediate percentage for herbaceous in stratum 2
    IMM_SIZE[which(mat.PFG.succ$type == "H" & STRATA > 2)] = 5 ## intermediate percentage for herbaceous in stratum > 2
    IMM_SIZE[which(mat.PFG.succ$type == "C" & STRATA == 1)] = 10 ## immature chamaephytes in 1st stratum contribute to shade in the same way than mature chamaephytes
    IMM_SIZE[which(mat.PFG.succ$type == "P" & mat.PFG.succ$height < 1000)] = 5 ## immature phanerophytes with height < 10m contribute to shade half less than mature phanerophytes
  }
  
  #################################################################################################
  
  if (doLightStrategy)
  {
    ## GET CHANGE STRATA AGES
    ## Logistic growth curve with 2 points to parameterize it :
    ## at age = maturity/2, height = IMM_SIZE * height	
    ## at age = longevity, height = height
    CHANG_STR_AGES = matrix(0, nrow = no.strata, ncol = no.PFG)
    if (no.strata > 1)
    {
      CHANG_STR_AGES[2:no.strata, ] = 10000
      for (i in 1:no.PFG)
      {
        ## If not in first stratum / herbaceous (or potentially chamaephytes) :
        if (!(IMM_SIZE[i] == 10))
        {
          k = -log(1 - IMM_SIZE[i] / 10) / (MATURITY[i] / 2)
          A = 1:LONGEVITY[i]
          
          ## negative binomiale curve
          H = mat.PFG.succ$height[i] * (1 - exp(-k * A))
          
          # calculation of transition ages depending on strata heights
          for (str in 2:no.strata) {
            age.brk = A[which(H >= STRATA_LIMITS[str])][1]
            CHANG_STR_AGES[str, i] = ifelse(is.na(age.brk), CHANG_STR_AGES[str, i], age.brk)
          }
        }
        # else if (mat.PFG.succ$height[i] > STRATA_LIMITS[2])
        # {
        #   CHANG_STR_AGES[2, i] = 0 ## direct into strata max
        # }
      }
    }
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
  if (sum(colnames(mat.PFG.succ) == "active_germ_low") == 1 ||
      sum(colnames(mat.PFG.succ) == "active_germ_medium") == 1 ||
      sum(colnames(mat.PFG.succ) == "active_germ_high") == 1)
  {
    if (sum(colnames(mat.PFG.succ) == "active_germ_low") == 1)
    {
      ACTIVE_GERM[1, ] = mat.PFG.succ$active_germ_low ## low light conditions
    }
    if (sum(colnames(mat.PFG.succ) == "active_germ_medium") == 1)
    {
      ACTIVE_GERM[2, ] = mat.PFG.succ$active_germ_medium ## low light conditions
    }
    if (sum(colnames(mat.PFG.succ) == "active_germ_high") == 1)
    {
      ACTIVE_GERM[3, ] = mat.PFG.succ$active_germ_high ## low light conditions
    }
  } else
  {
    ## woody species have little variation in germination rate depending on light conditions
    ACTIVE_GERM[, which(mat.PFG.succ$type %in% c("C", "P"))] = 9
    ## herbaceous germinate less in the shadow
    ACTIVE_GERM[1, which(mat.PFG.succ$type == "H")] = 5 ## low light conditions
    ACTIVE_GERM[2, which(mat.PFG.succ$type == "H")] = 8 ## medium light conditions
    ACTIVE_GERM[3, which(mat.PFG.succ$type == "H")] = 9 ## high light conditions
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

  SHADE_TOL = matrix(0, nrow = 3 * 3, ncol = no.PFG)

  if(doLightStrategy)
  {
    for (i in 1:no.PFG){
      SHADE_TOL[, i] = switch(mat.PFG.succ$light[i]
                              , full_light = c(1,1,1,0,0,1,0,0,1)
                              , pioneer = c(1,1,1,0,1,1,0,1,1)
                              , ubiquist = c(1,1,1,1,1,1,1,1,1)
                              , semi_shade = c(1,1,0,1,1,0,1,1,1)
                              , undergrowth = c(1,1,0,1,1,0,1,1,0)
      )
    }
  } else
  {
    for (i in 1:no.PFG){
      ## Low light condition
      if (mat.PFG.succ$light[i] <= 2)
      {
        SHADE_TOL[c(1, 4, 7), i] = 1
      }
      ## Medium light condition
      if (mat.PFG.succ$light[i] >= 2 && mat.PFG.succ$light[i] <= 4)
      {
        SHADE_TOL[c(2, 5, 8), i] = 1
      }
      ## High light condition
      if (mat.PFG.succ$light[i] >= 3)
      {
        SHADE_TOL[c(3, 6, 9), i] = 1
      }
    }
    
    ## All germinants are assumed to be tolerant to Low light
    SHADE_TOL[c(1),] = 1
    ## All mature trees and shrubs are assumed to be tolerant to Low and Medium Light
    SHADE_TOL[c(8, 9), which(mat.PFG.succ$type %in% c("C", "P"))] = 1
    ## All immature trees that grow in the penultimate stratum are assumed to be tolerant to High light
    SHADE_TOL[c(6), which(mat.PFG.succ$type == "P" & CHANG_STR_AGES[nrow(CHANG_STR_AGES) - 1,] < MATURITY)] = 1
    
    ## What about all germinant tolerant to Medium light ?
    ## What about all mature trees and shrubs tolerant to Low light ?
  }
  
  #################################################################################################

  names.params.list = get("NAME")
  names.params.list.sub = c("NAME"
                            , "LIGHT"
                            , "ACTIVE_GERM"
                            , "SHADE_TOL")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "LIGHT"
                           , paste0("ACTIVE_GERM_for_", c("L", "M", "H"))
                           , paste0("SHADE_TOL_for_",
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

