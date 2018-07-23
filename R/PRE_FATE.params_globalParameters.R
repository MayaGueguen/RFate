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
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{FATE-HD} simulation
##' @param required.no_PFG an \code{integer} corresponding to the number of PFG
##' @param required.no_STRATA an \code{integer} corresponding to the number of 
##' height strata
##' @param required.succ_option a \code{string} to choose if the simulation will
##'  include habitat suitability (\emph{fateh}) or not (\emph{fate})
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
##' @param DIST.no the number of disturbances
##' @param DIST.no_sub the number of way a PFG could react to a disturbance
##' @param DIST.freq the frequency of each disturbance (\emph{in years})
##' @param doSoil \emph{to be filled}
##' @param doFire \emph{to be filled}
##' @param doDrought \emph{to be filled}
##' @param doHabStability \emph{to be filled}
##' @param doAliens \emph{to be filled}
##' 
##' 
##' 
##' 
##' @details 
##' 
##' The core module of \code{FATE-HD} requires several parameters to define 
##' general characteristics of the simulation :
##' 
##' \describe{
##'   \item{Studied system}{
##'   \itemize{
##'     \item \emph{required.no_PFG} : the number of Plant Functional Groups 
##'     that will be included into the simulation.  
##'     This number should match with the number of files that will be given 
##'     with the characteristics of each group (SUCC, DISP, ...).
##'     \item \emph{required.no_STRATA} : the number of height strata that 
##'     will be used into the succession module.  
##'     This number should match with the maximum number of strata possible
##'     defined into the PFG SUCC files.
##'   }
##'   }
##'   \item{Habitat suitability}{
##'   \itemize{
##'     \item \emph{required.succ_option} : if the simulation will include 
##'     habitat suitability filter (\emph{fateh}) or not (\emph{fate}). \cr
##'     This filter is based on maps given for each PFG, with values between 
##'     0 and 1 corresponding to the probability of presence of the group in 
##'     each pixel. Each year (timestep), this value will be compared to a 
##'     reference value, and if superior, the PFG will be able to grow and
##'     survive.
##'     \item \emph{required.hs_option} : the habitat suitability reference
##'     value can be set in two possible ways :
##'     \enumerate{
##'       \item \emph{random} : for each pixel, the reference value is drawn from
##'       a uniform distribution, and the same value is used for each PFG 
##'       within this pixel.
##'       \item \emph{PFG specific} : for each PFG, a mean value and a standard 
##'       deviation value are drawn from a uniform distribution. For each 
##'       pixel and for each PFG, the reference value is drawn from a normal 
##'       distribution of parameters the mean and standard deviation of the PFG.
##'     }
##'   }
##'   }
##'   \item{Simulation timing}{
##'   \itemize{
##'     \item \emph{required.seeding_timestep} : the time interval at which 
##'     occurs the seeding, and until the seeding duration is not over 
##'     (\emph{in years})
##'     \item \emph{required.seeding_duration} : the duration of seeding 
##'     (\emph{in years})
##'     \item \emph{required.simul_duration} : the duration of simulation 
##'     (\emph{in years}) \cr \cr
##'   }
##'   }
##' }
##' 
##' 
##' The other modules of \code{FATE-HD} can be activated within this file, and
##' if so, some additional parameters will be required :
##' 
##' \describe{
##'   \item{DISTURBANCES}{= defined for events such as mowing, grazing, but also
##'   urbanization, crops, etc. \cr \cr
##'   The impact zone is defined with a mask (map with 0 or 1) and the user will
##'   have to define how each PFG will be impacted depending on age and life stage. 
##'   \itemize{
##'     \item \emph{DIST.no} : the number of different disturbances
##'     \item \emph{DIST.no_sub} : the number of way a PFG could react to a 
##'     perturbation
##'     \item \emph{DIST.freq} : the frequency of each disturbance (\emph{in years})
##'   }
##'   }
##'   \item{DROUGHT}{= to experience extreme events with a direct and a delayed
##'   response on PFG. \cr \cr
##'   The PFG's past drought exposure is compared to current moisture values as 
##'   a proxy of drought index. Developed canopy closure helps to reduce these 
##'   values. Each PFG is impacted if submitted one year to drought, and also (and  
##'   more consequently) if submitted several years in a row.
##'   \itemize{
##'     \item \emph{DROUGHT.no_sub} : the number of way a PFG could react to a 
##'     perturbation
##'     \item \emph{DROUGHT.chrono_post} : if post-drought effects will occur 
##'     previously to succession (0) or after (1)
##'     \item \emph{DROUGHT.chrono_curr} : if current-drought effects will occur 
##'     previously to succession (0) or after (1)
##'   }
##'   }
##'   \item{HABITAT STABILITY}{= to evaluate through the evolution of the PFG 
##'   composition in different habitats. \cr \cr
##'   The habitat distribution is given with a categorical map with each value
##'   corresponding to a specific habitat. Every year, the abundance of each PFG
##'   within each habitat is recorded, as well as the evenness of the habitat :
##'   \deqn{
##'   \text{evenness} = - SUM(\text{PFG.proportion} * log(\text{PFG.proportion})) / log(\text{no.PFG} - 1)
##'   }
##'   \deqn{\text{with PFG.proportion} = \text{abund.PFG.in.Habitat} / \text{abund.allPFG.in.Habitat}}
##'   Every 5 years, a stability check is done to see if the abundance or the
##'   evenness have values out of the usual distribution.
##'   \itemize{
##'     \item \emph{HABSTAB.no_hab} : the number of different habitats
##'   }
##'   }
##'   \item{INVASIVE \cr INTRODUCTION}{= to add new PFG during the simulation. \cr \cr
##'   The introduction areas are defined with a mask (map with 0 or 1). If the 
##'   habitat suitability filter is on, suitability maps will also be needed for
##'   these new groups.
##'   \itemize{
##'     \item \emph{ALIEN.freq} : the frequency of each introduction (\emph{in years})
##'   }
##'   }
##' }
##' 
##' 
##' @return A \code{.txt} file into the \code{name.simulation/DATA/GLOBAL_PARAMETERS}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item NB_CPUS
##'   \item NB_FG
##'   \item NB_STRATUM
##'   \item SUCC_MOD
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
##'                                  , required.no_PFG = 3
##'                                  , required.no_STRATA = 5
##'                                  , required.succ_option = "fateh"
##'                                  , required.hs_option = 1
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_duration = 10
##'                                  , required.simul_duration = 100)
##'                                    
##' ## Create SEVERAL Global_parameters files
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 3
##'                                  , required.no_STRATA = 5
##'                                  , required.succ_option = "fateh"
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
  , opt.no_CPU = 1
  , required.no_PFG
  , required.no_STRATA
  , required.succ_option
  , required.hs_option
  , required.seeding_timestep
  , required.seeding_duration
  , required.simul_duration
  , doDisturbances = FALSE
  , DIST.no
  , DIST.no_sub
  , DIST.freq
  , doSoil = FALSE
  # , SOIL.def_value
  # , SOIL.no_categories
  # , SOIL.tresh_categories
  , doFire = FALSE
  # , FIRE.no
  # , FIRE.no_sub
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
  # , FIRE.DI_no_clim
  , doDrought = FALSE
  # , DROUGHT.no_sub
  # , DROUGHT.chrono_post
  # , DROUGHT.chrono_curr
  , doHabStability = FALSE
  # , HABSTAB.no_hab
  , doAliens = FALSE
  # , ALIEN.freq
){
  
  if (.testParam_notChar(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/DATA/GLOBAL_PARAMETERS/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  }
  if (is.na(opt.no_CPU) ||
      is.null(opt.no_CPU) ||
      !is.numeric(opt.no_CPU) ||
      opt.no_CPU <= 0){
    warning(paste0("Wrong type of data!\n `opt.no_CPU` must be an integer > 0\n"
                   , " ==> Automatically set to 1"))
  }
  if (.testParam_notNum(required.no_PFG) ||
      required.no_PFG <= 0 ){
    stop("Wrong type of data!\n `required.no_PFG` must be an integer > 0")
  }
  if (.testParam_notNum(required.no_STRATA) ||
      required.no_STRATA <= 0 ){
    stop("Wrong type of data!\n `required.no_STRATA` must be an integer > 0")
  }
  if (.testParam_notInChar(required.succ_option, inList = c("fate", "fateh")))
  {
    stop("Wrong type of data!\n `required.succ_option` must be either `fate` or `fateh` (habitat suitability)")
  }
  if (.testParam_notNum(required.hs_option) ||
      !(required.hs_option %in% c(1,2))){
    stop("Wrong type of data!\n `required.hs_option` must be either 1 (random) or 2 (distribution per PFG)")
  }
  if (.testParam_notNum(required.seeding_timestep) ||
      required.seeding_timestep <= 0 ){
    stop("Wrong type of data!\n `required.seeding_timestep` must be an integer > 0")
  }
  if (.testParam_notNum(required.seeding_duration) ||
      required.seeding_duration <= 0 ){
    stop("Wrong type of data!\n `required.seeding_duration` must be an integer > 0")
  }
  if (.testParam_notNum(required.simul_duration) ||
      required.simul_duration <= 0 ){
    stop("Wrong type of data!\n `required.simul_duration` must be an integer > 0")
  }
  if (doDisturbances)
  {
    if (.testParam_notNum(DIST.no) ||
        DIST.no <= 0 ){
      stop("Wrong type of data!\n `DIST.no` must be an integer > 0")
    }
    if (.testParam_notNum(DIST.no_sub) ||
        DIST.no_sub <= 0 ){
      stop("Wrong type of data!\n `DIST.no_sub` must be an integer > 0")
    }
    if (.testParam_notNum(DIST.freq) ||
        sum(DIST.freq <= 0) > 0){
      stop("Wrong type of data!\n `DIST.freq` must be a vector of integer > 0")
    } else if (length(DIST.freq) != DIST.no){
      stop("Wrong type of data!\n `DIST.freq` must contain as many values as the number of disturbances (`DIST.no`)")
    }
  }

  #################################################################################################
  
  if (doDisturbances)
  {
    params.DIST = list(as.numeric(doDisturbances)
                       , DIST.no
                       , DIST.no_sub
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
    # params.SOIL = list(as.numeric(doSoil)
    #                    , SOIL.def_value
    #                    , SOIL.no_categories
    #                    , SOIL.tresh_categories)
    # names.params.list.SOIL = c("DO_SOIL_COMPETITION"
    #                            , "SOIL_DEFAULT_VALUE"
    #                            , "NB_SOIL_CATEGORIES"
    #                            , "SOIL_CATEGORIES_TRESHOLDS")
  } else
  {
    params.SOIL = list(as.numeric(doSoil))
    names.params.list.SOIL = "DO_SOIL_COMPETITION"
  }
  if (doDrought)
  {
    # params.DROUGHT = list(as.numeric(doDrought)
    #                       , DROUGHT.no_sub
    #                       , DROUGHT.chrono_post
    #                       , DROUGHT.chrono_curr)
    # names.params.list.DROUGHT = c("DO_DROUGHT_DISTURBANCES"
    #                               , "NB_SUBDROUGHT"
    #                               , "CHRONO_POST_DROUGHT"
    #                               , "CHRONO_CURR_DROUGHT")
  } else
  {
    params.DROUGHT = list(as.numeric(doDrought))
    names.params.list.DROUGHT = "DO_DROUGHT_DISTURBANCES"
  }
  if (doHabStability)
  {
    # params.HABSTAB = list(as.numeric(doHabStability)
    #                       , HABSTAB.no_hab)
    # names.params.list.HABSTAB = c("DO_HAB_STABILITY"
    #                               , "NB_HABITATS")
  } else
  {
    params.HABSTAB = list(as.numeric(doHabStability))
    names.params.list.HABSTAB = "DO_HAB_STABILITY"
  }
  if (doAliens)
  {
    # params.ALIEN = list(as.numeric(doAliens)
    #                     , ALIEN.freq)
    # names.params.list.ALIEN = c("DO_ALIENS_DISTURBANCE"
    #                             , "FREQ_ALIENS")
  } else
  {
    params.ALIEN = list(as.numeric(doAliens))
    names.params.list.ALIEN = "DO_ALIENS_DISTURBANCE"
  } 

  #################################################################################################
  
  params.combi = expand.grid(opt.no_CPU
                             , required.no_PFG
                             , required.no_STRATA
                             , required.succ_option
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
                            , "NB_FG"
                            , "NB_STRATUM"
                            , "SUCC_MOD"
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
