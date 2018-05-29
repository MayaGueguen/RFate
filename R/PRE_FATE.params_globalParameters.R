### HEADER #####################################################################
##' @title Create \emph{Global_parameters} parameter file for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_globalParameters
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create one parameter file
##' containing \code{GLOBAL PARAMETERS} used in \code{FATE-HD} model.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param opt.nb_CPU default 1 (\emph{optional}). The number of resources that can be used 
##' to parallelize the \code{FATE-HD} simulation
##' @param required.succ_option a \code{string} to choose if the simulation will
##'  include habitat suitability (\emph{fateh}) or not (\emph{fate})
##' @param required.nb_PFG an \code{integer} corresponding to the number of PFG
##' @param required.nb_STRATA an \code{integer} corresponding to the number of 
##' height strata
##' @param required.hs_option an \code{integer} corresponding to the way of 
##' simulating the habitat suitability variation between years for each PFG, 
##' either random (1) or PFG specific (2)
##' @param required.seeding_timestep an \code{integer} corresponding to the 
##' time interval at which occurs the seeding, and until the seeding duration
##'  is not over (\emph{in years})
##' @param required.seeding_duration an \code{integer} corresponding to the 
##' duration of seeding (\emph{in years})
##' @param required.simul_duration an \code{integer} corresponding to the 
##' duration of simulation (\emph{in years})
##' @param doDisturbances default \code{FALSE}. If \code{TRUE}, disturbances 
##' are applied in the \code{FATE-HD} simulation, and associated parameters 
##' are required
##' @param DIST.nb the number of disturbances
##' @param DIST.nb_sub the number of way a PFG should react to a disturbance
##' @param DIST.freq the frequency of each disturbance (\emph{in years})
##' 
##' 
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
##' @return A \code{.txt} file into the \code{name.simulation/DATA/GLOBAL_PARAMETERS}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item NB_CPUS
##'   \item SUCC_MOD
##'   \item NB_FG
##'   \item NB_STRATUM
##'   \item ENVSUIT_OPTION
##'   \item SEEDING_TIMESTEP
##'   \item SEEDING_DURATION
##'   \item SIMULATION_TIME
##' }
##' 
##' If the simulation includes disturbances :
##' 
##' \itemize{
##'   \item DO_DISTURBANCES
##'   \item NB_DISTURBANCES
##'   \item NB_SUBDISTURBANCES
##'   \item FREQ_DISTURBANCES
##' }
##' 
##' If the simulation includes soil competition :
##' 
##' \itemize{
##'   \item DO_SOIL_COMPETITION
##'   \item SOIL_DEFAULT_VALUE
##'   \item NB_SOIL_CATEGORIES
##'   \item SOIL_CATEGORIES_TRESHOLDS
##' }
##' 
##' If the simulation includes drought disturbances :
##' 
##' \itemize{
##'   \item DO_DROUGHT_DISTURBANCES
##'   \item NB_SUBDROUGHT
##'   \item CHRONO_POST_DROUGHT
##'   \item CHRONO_CURR_DROUGHT
##' }
##' 
##' If the simulation includes habitat stability check :
##' 
##' \itemize{
##'   \item DO_HAB_STABILITY
##'   \item NB_HABITATS
##' }
##' 
##' If the simulation includes aliens introduction :
##' 
##' \itemize{
##'   \item DO_ALIENS_DISTURBANCE
##'   \item FREQ_ALIENS
##' }
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.succ_option = "fateh"
##'                                  , required.nb_PFG = 3
##'                                  , required.nb_STRATA = 5
##'                                  , required.hs_option = 1
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_duration = 10
##'                                  , required.simul_duration = 100)
##'                                    
##' ## Create SEVERAL Global_parameters files
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.succ_option = "fateh"
##'                                  , required.nb_PFG = 3
##'                                  , required.nb_STRATA = 5
##'                                  , required.hs_option = c(1,2)
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_duration = 10
##'                                  , required.simul_duration = 100)
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_globalParameters = function(
  name.simulation
  , opt.nb_CPU = 1
  , required.succ_option
  , required.nb_PFG
  , required.nb_STRATA
  , required.hs_option
  , required.seeding_timestep
  , required.seeding_duration
  , required.simul_duration
  , doDisturbances = FALSE
  , DIST.nb
  , DIST.nb_sub
  , DIST.freq
  , doSoil = FALSE
  # , SOIL.def_value
  # , SOIL.nb_categories
  # , SOIL.tresh_categories
  , doFire = FALSE
  # , FIRE.nb
  # , FIRE.nb_sub
  # , FIRE.freq
  # , FIRE.init_option
  # , FIRE.init_nb
  # , FIRE.init_nb_prev
  # , FIRE.neigh_option
  # , FIRE.neigh_cc_extent
  # , FIRE.prop_option
  # , FIRE.prop_prob
  # , FIRE.quota_option
  # , FIRE.flamm_max
  # , FIRE.logit_init
  # , FIRE.logit_spread
  # , FIRE.DI_nb_clim
  , doDrought = FALSE
  # , DROUGHT.nb_sub
  # , DROUGHT.chrono_post
  # , DROUGHT.chrono_curr
  , doHabStability = FALSE
  # , HABSTAB.nb_hab
  , doAliens = FALSE
  # , ALIEN.freq
){
  
  if (missing(name.simulation) ||
      is.na(name.simulation) ||
      is.null(name.simulation) ||
      !is.character(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/GLOBAL_PARAMETERS/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  }
  if (is.na(opt.nb_CPU) ||
      is.null(opt.nb_CPU) ||
      !is.numeric(opt.nb_CPU) ||
      opt.nb_CPU <= 0){
    warning(paste0("Wrong type of data!\n `opt.nb_CPU` must be an integer > 0\n"
                   , " ==> Automatically set to 1"))
  }
  if (missing(required.succ_option) ||
      is.na(required.succ_option) ||
      is.null(required.succ_option) ||
      !(required.succ_option %in% c("fate", "fateh"))){
    stop("Wrong type of data!\n `required.succ_option` must be either `fate` or `fateh` (habitat suitability)")
  }
  if (missing(required.nb_PFG) ||
      is.na(required.nb_PFG) ||
      is.null(required.nb_PFG) ||
      !is.numeric(required.nb_PFG) ||
      required.nb_PFG <= 0 ){
    stop("Wrong type of data!\n `required.nb_PFG` must be an integer > 0")
  }
  if (missing(required.nb_STRATA) ||
      is.na(required.nb_STRATA) ||
      is.null(required.nb_STRATA) ||
      !is.numeric(required.nb_STRATA) ||
      required.nb_STRATA <= 0 ){
    stop("Wrong type of data!\n `required.nb_STRATA` must be an integer > 0")
  }
  if (missing(required.hs_option) ||
      is.na(required.hs_option) ||
      is.null(required.hs_option) ||
      !is.numeric(required.hs_option) ||
      !(required.hs_option %in% c(1,2))){
    stop("Wrong type of data!\n `required.hs_option` must be either 1 (random) or 2 (distribution per PFG)")
  }
  if (missing(required.seeding_timestep) ||
      is.na(required.seeding_timestep) ||
      is.null(required.seeding_timestep) ||
      !is.numeric(required.seeding_timestep) ||
      required.seeding_timestep <= 0 ){
    stop("Wrong type of data!\n `required.seeding_timestep` must be an integer > 0")
  }
  if (missing(required.seeding_duration) ||
      is.na(required.seeding_duration) ||
      is.null(required.seeding_duration) ||
      !is.numeric(required.seeding_duration) ||
      required.seeding_duration <= 0 ){
    stop("Wrong type of data!\n `required.seeding_duration` must be an integer > 0")
  }
  if (missing(required.simul_duration) ||
      is.na(required.simul_duration) ||
      is.null(required.simul_duration) ||
      !is.numeric(required.simul_duration) ||
      required.simul_duration <= 0 ){
    stop("Wrong type of data!\n `required.simul_duration` must be an integer > 0")
  }
  if (doDisturbances)
  {
    if (missing(DIST.nb) ||
        is.na(DIST.nb) ||
        is.null(DIST.nb) ||
        !is.numeric(DIST.nb) ||
        DIST.nb <= 0 ){
      stop("Wrong type of data!\n `DIST.nb` must be an integer > 0")
    }
    if (missing(DIST.nb_sub) ||
        is.na(DIST.nb_sub) ||
        is.null(DIST.nb_sub) ||
        !is.numeric(DIST.nb_sub) ||
        DIST.nb_sub <= 0 ){
      stop("Wrong type of data!\n `DIST.nb_sub` must be an integer > 0")
    }
    if (missing(DIST.freq) ||
        is.na(DIST.freq) ||
        is.null(DIST.freq) ||
        !is.numeric(DIST.freq) ||
        sum(DIST.freq <= 0) < length(DIST.freq)){
      stop("Wrong type of data!\n `DIST.freq` must be a vector of integer > 0")
    } else if (length(DIST.freq) != DIST.nb){
      stop("Wrong type of data!\n `DIST.freq` must contain as many values as the number of disturbances (`DIST.nb`)")
    }
  }

  #################################################################################################
  
  if (doDisturbances)
  {
    params.DIST = list(as.numeric(doDisturbances)
                       , DIST.nb
                       , DIST.nb_sub
                       , DIST.freq)
    names.params.list.DIST = c("DO_DISTURBANCES"
                               , "NB_DISTURBANCES"
                               , "NB_SUBDISTURBANCES"
                               , "FREQ_DISTURBANCES")
  } else
  {
    params.DIST = list(as.numeric(doDisturbances))
    names.params.list.DIST = "DO_DISTURBANCES"
  }
  if (doSoil)
  {
    params.SOIL = list(as.numeric(doSoil)
                       , SOIL.def_value
                       , SOIL.nb_categories
                       , SOIL.tresh_categories)
    names.params.list.SOIL = c("DO_SOIL_COMPETITION"
                               , "SOIL_DEFAULT_VALUE"
                               , "NB_SOIL_CATEGORIES"
                               , "SOIL_CATEGORIES_TRESHOLDS")
  } else
  {
    params.SOIL = list(as.numeric(doSoil))
    names.params.list.SOIL = "DO_SOIL_COMPETITION"
  }
  if (doDrought)
  {
    params.DROUGHT = list(as.numeric(doDrought)
                          , DROUGHT.nb_sub
                          , DROUGHT.chrono_post
                          , DROUGHT.chrono_curr)
    names.params.list.DROUGHT = c("DO_DROUGHT_DISTURBANCES"
                                  , "NB_SUBDROUGHT"
                                  , "CHRONO_POST_DROUGHT"
                                  , "CHRONO_CURR_DROUGHT")
  } else
  {
    params.DROUGHT = list(as.numeric(doDrought))
    names.params.list.DROUGHT = "DO_DROUGHT_DISTURBANCES"
  }
  if (doHabStability)
  {
    params.HABSTAB = list(as.numeric(doHabStability)
                          , HABSTAB.nb_hab)
    names.params.list.HABSTAB = c("DO_HAB_STABILITY"
                                  , "NB_HABITATS")
  } else
  {
    params.HABSTAB = list(as.numeric(doHabStability))
    names.params.list.HABSTAB = "DO_HAB_STABILITY"
  }
  if (doAliens)
  {
    params.ALIEN = list(as.numeric(doAliens)
                        , ALIEN.freq)
    names.params.list.ALIEN = c("DO_ALIENS_DISTURBANCE"
                                , "FREQ_ALIENS")
  } else
  {
    params.ALIEN = list(as.numeric(doAliens))
    names.params.list.ALIEN = "DO_ALIENS_DISTURBANCE"
  } 

  #################################################################################################
  
  params.combi = expand.grid(opt.nb_CPU
                             , required.succ_option
                             , required.nb_PFG
                             , required.nb_STRATA
                             , required.hs_option
                             , required.seeding_timestep
                             , required.seeding_duration
                             , required.simul_duration)
  
  params.list = lapply(1:nrow(params.combi), function(x) {
    res = lapply(1:ncol(params.combi), function(y) { params.combi[x, y] })
    res = c(res, params.DIST)
    res = c(res, params.SOIL)
    res = c(res, params.DROUGHT)
    res = c(res, params.HABSTAB)
    res = c(res, params.ALIEN)
  })
  names.params.list = paste0("V", 1:length(params.list))
  names.params.list.sub = c("NB_CPUS"
                            , "SUCC_MOD"
                            , "NB_FG"
                            , "NB_STRATUM"
                            , "ENVSUIT_OPTION"
                            , "SEEDING_TIMESTEP"
                            , "SEEDING_DURATION"
                            , "SIMULATION_TIME")
  names.params.list.sub = c(names.params.list.sub, names.params.list.DIST)
  names.params.list.sub = c(names.params.list.sub, names.params.list.SOIL)
  names.params.list.sub = c(names.params.list.sub, names.params.list.DROUGHT)
  names.params.list.sub = c(names.params.list.sub, names.params.list.HABSTAB)
  names.params.list.sub = c(names.params.list.sub, names.params.list.ALIEN)
  
  
  for (i in 1:length(params.list)){
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/GLOBAL_PARAMETERS/Global_parameters_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
}
