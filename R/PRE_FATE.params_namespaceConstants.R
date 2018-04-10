### HEADER #####################################################################
##' @title Create \emph{Namespace_constants} parameter file for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_namespaceConstants
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to 
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
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
##'   \item GLOBAL_MEDIUM_RESOURCES_TRESH
##'   \item GLOBAL_LOW_RESOURCES_TRESH
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
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_namespaceConstants = function(
  name.simulation
  , global.abund.low
  , global.abund.med
  , global.abund.high
  , global.max.by.cohort
  , global.resource.thresh.med
  , global.resource.thresh.low
){
  
  params.list = list(global.abund.low
                     , global.abund.med
                     , global.abund.high
                     , global.max.by.cohort
                     , global.resource.thresh.med
                     , global.resource.thresh.low
                     , 9000000)
  names(params.list) = c("GLOBAL_LOW_ABUND"
                         , "GLOBAL_MEDIUM_ABUND"
                         , "GLOBAL_HIGH_ABUND"
                         , "GLOBAL_MAX_BY_COHORT"
                         , "GLOBAL_MEDIUM_RESOURCES_TRESH"
                         , "GLOBAL_LOW_RESOURCES_TRESH"
                         , "GLOBAL_FULL_SOIL_COVERAGE")
  .createParams(params.file = paste0(name.simulation, "/DATA/Namespace_constants.txt")
                , params.list = params.list)

}
  
