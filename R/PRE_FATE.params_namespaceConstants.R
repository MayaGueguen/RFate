### HEADER #####################################################################
##' @title Create \emph{Namespace_constants} parameter file for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_namespaceConstants
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create one (or several) parameter file
##' containing \code{NAMESPACE CONSTANTS} used in \code{FATE-HD} model.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param opt.replacePrevious default \code{FALSE} (\emph{optional}). If \code{TRUE},
##' pre-existing files inside \code{name.simulation/DATA/NAMESPACE_CONSTANTS} folder
##' will be replaced
##' @param global.abund.low \emph{Not used for now. To be removed ?}
##' @param global.abund.med \emph{Not used for now. To be removed ?}
##' @param global.abund.high an \code{integer} in the order of 1 000 000 to rescale
##' abundance values in each pixel
##' @param global.max.by.cohort an \code{integer} in the order of 1 000 000 to rescale
##' abundance values of each cohort in each pixel
##' @param global.resource.thresh.med an \code{integer} in the order of 1 000 000
##' to convert PFG abundances in each strata into light resources. It corresponds to 
##' the limit of abundances above which light resources are \code{medium}.
##' PFG abundances lower than this threshold imply \strong{high amount of light}.
##' Is is consequently lower than \code{global.resource.thresh.low}.
##' @param global.resource.thresh.low an \code{integer} in the order of 1 000 000
##' to convert PFG abundances in each strata into light resources. It corresponds to 
##' the limit of abundances above which light resources are \code{low}.
##' PFG abundances higher than \code{global.resource.thresh.med} and lower than this
##' threshold imply \strong{medium amount of light}.
##' 
##' 
##' @details 
##' 
##' \describe{
##'   \item{To get abundances \cr per PFG per pixel}{
##'   \itemize{
##'     \item per strata
##'     \item for all strata
##'   }
##'   \deqn{abund = 10 000 * totalAbund / \text{GLOBAL HIGH ABUND}}
##'   }
##'   \item{To transform PFG \cr abundances into \cr light resources}{
##'     \deqn{abund < \text{GLOBAL MEDIUM RESOURCES THRESH} \Leftrightarrow Light = High}
##'     \deqn{abund > \text{GLOBAL MEDIUM RESOURCES THRESH } \& \\
##'     abund < \text{GLOBAL LOW RESOURCES THRESH} \Leftrightarrow Light = Medium}
##'     \deqn{abund > \text{GLOBAL LOW RESOURCES THRESH} \Leftrightarrow Light = Low}
##'   }
##' }
##' 
##' 
##' @return A \code{.txt} file into the \code{name.simulation/DATA/NAMESPACE_CONSTANTS}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item GLOBAL_LOW_ABUND
##'   \item GLOBAL_MEDIUM_ABUND
##'   \item GLOBAL_HIGH_ABUND
##'   \item GLOBAL_MAX_BY_COHORT
##'   \item GLOBAL_MEDIUM_RESOURCES_THRESH
##'   \item GLOBAL_LOW_RESOURCES_THRESH
##'   \item GLOBAL_FULL_SOIL_COVERAGE
##' }
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Namespace_constants parameter file
##' PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
##'                                    , global.abund.low = 1000000
##'                                    , global.abund.med = 5000000
##'                                    , global.abund.high = 8000000
##'                                    , global.max.by.cohort = 5000000
##'                                    , global.resource.thresh.med = 13000000
##'                                    , global.resource.thresh.low = 19000000)
##'                                    
##' ## Create SEVERAL Namespace_constants parameter file
##' PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
##'                                    , global.abund.low = 1000000
##'                                    , global.abund.med = 5000000
##'                                    , global.abund.high = c(8000000, 9000000)
##'                                    , global.max.by.cohort = 5000000
##'                                    , global.resource.thresh.med = 13000000
##'                                    , global.resource.thresh.low = 19000000)
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_namespaceConstants = function(
  name.simulation
  , opt.replacePrevious = FALSE
  , global.abund.low
  , global.abund.med
  , global.abund.high
  , global.max.by.cohort
  , global.resource.thresh.med
  , global.resource.thresh.low
){
  
  .testParam_existFolder(name.simulation, "DATA/NAMESPACE_CONSTANTS/")
  
  if (.testParam_notNum(global.abund.low))
  {
    .stopMessage_beInteger("global.abund.low")
  } else
  {
    if (round(global.abund.low) != global.abund.low)
    {
      warning("`global.abund.low` is a double. It will be converted (rounded) to an integer")
    }
  }
  if (.testParam_notNum(global.abund.med))
  {
    .stopMessage_beInteger("global.abund.med")
  } else
  {
    if (round(global.abund.med) != global.abund.med)
    {
      warning("`global.abund.med` is a double. It will be converted (rounded) to an integer")
    }
  }
  if (.testParam_notNum(global.abund.high))
  {  
    .stopMessage_beInteger("global.abund.high")
  } else
  {
    if (round(global.abund.high) != global.abund.high)
    {
      warning("`global.abund.high` is a double. It will be converted (rounded) to an integer")
    }
  }
  if (.testParam_notNum(global.max.by.cohort))
  {
    .stopMessage_beInteger("global.max.by.cohort")
  } else
  {
    if (round(global.max.by.cohort) != global.max.by.cohort)
    {
      warning("`global.max.by.cohort` is a double. It will be converted (rounded) to an integer")
    }
  }
  if (.testParam_notNum(global.resource.thresh.med))
  {
    .stopMessage_beInteger("global.resource.thresh.med")
  } else
  {
    if (round(global.resource.thresh.med) != global.resource.thresh.med)
    {
      warning("`global.resource.thresh.med` is a double. It will be converted (rounded) to an integer")
    }
  }
  if (.testParam_notNum(global.resource.thresh.low))
  {
    .stopMessage_beInteger("global.resource.thresh.low")
  } else
  {
    if (round(global.resource.thresh.low) != global.resource.thresh.low)
    {
      warning("`global.resource.thresh.low` is a double. It will be converted (rounded) to an integer")
    }
  }
  
  params.combi = expand.grid(as.integer(global.abund.low)
                             , as.integer(global.abund.med)
                             , as.integer(global.abund.high)
                             , as.integer(global.max.by.cohort)
                             , as.integer(global.resource.thresh.med)
                             , as.integer(global.resource.thresh.low)
                             , as.integer(9000000))
  params.list = lapply(1:nrow(params.combi), function(x) {
    list(params.combi[x, 1]
         , params.combi[x, 2]
         , params.combi[x, 3]
         , params.combi[x, 4]
         , params.combi[x, 5]
         , params.combi[x, 6]
         , params.combi[x, 7])
  })

  no.start = 1
  if (!opt.replacePrevious)
  {
    previous.files = list.files(path = paste0(name.simulation, "/DATA/NAMESPACE_CONSTANTS/")
                                , pattern = "^Namespace_constants_")
    if (length(previous.files) > 0) {
      no.start = length(previous.files) + 1
    }
  }

  names.params.list = paste0("V", no.start:length(params.list))
  names.params.list.sub = c("GLOBAL_LOW_ABUND"
                            , "GLOBAL_MEDIUM_ABUND"
                            , "GLOBAL_HIGH_ABUND"
                            , "GLOBAL_MAX_BY_COHORT"
                            , "GLOBAL_MEDIUM_RESOURCES_THRESH"
                            , "GLOBAL_LOW_RESOURCES_THRESH"
                            , "GLOBAL_FULL_SOIL_COVERAGE")
  
  for (i in 1:length(params.list)){
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/NAMESPACE_CONSTANTS/Namespace_constants_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
}
