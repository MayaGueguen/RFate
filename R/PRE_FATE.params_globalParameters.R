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
##' @param name.simulation a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{FATE-HD} simulation
##' @param opt.replacePrevious default \code{FALSE} (\emph{optional}). If 
##' \code{TRUE}, pre-existing files inside
##' \code{name.simulation/DATA/GLOBAL_PARAMETERS} folder will be replaced
##' @param required.no_PFG an \code{integer} corresponding to the number of PFG
##' @param required.no_STRATA an \code{integer} corresponding to the number of 
##' height strata
##' @param required.simul_duration an \code{integer} corresponding to the 
##' duration of simulation (\emph{in years})
##' @param required.seeding_duration an \code{integer} corresponding to the 
##' duration of seeding (\emph{in years})
##' @param required.seeding_timestep an \code{integer} corresponding to the 
##' time interval at which occurs the seeding, and until the seeding duration
##' is not over (\emph{in years})
##' @param required.seeding_input an \code{integer} corresponding to the 
##' number of seeds attributed to each PFG at each time step, and until the
##' seeding duration is not over
##' @param required.max_by_cohort an \code{integer} in the order of 1 000 000
##' to rescale abundance values of each cohort in each pixel (carrying capacity
##' equivalent)
##' @param required.max_abund_low an \code{integer} in the order of 1 000 000
##' to rescale abundance values of small PFG
##' @param required.max_abund_medium an \code{integer} in the order of 1 000 000
##' to rescale abundance values of intermediate PFG
##' @param required.max_abund_high an \code{integer} in the order of 1 000 000
##' to rescale abundance values of tall PFG
##' @param doLight default \code{FALSE}. If \code{TRUE}, light competition is 
##' activated in the \code{FATE-HD} simulation, and associated parameters are 
##' required
##' @param LIGHT.thresh_medium an \code{integer} in the order of 1 000 000
##' to convert PFG abundances in each strata into light resources. It
##' corresponds to the limit of abundances above which light resources are
##' \code{medium}. PFG abundances lower than this threshold imply \strong{high
##' amount of light}. It is consequently lower than \code{LIGHT.thresh_low}.
##' @param LIGHT.thresh_low an \code{integer} in the order of 1 000 000
##' to convert PFG abundances in each strata into light resources. It
##' corresponds to the limit of abundances above which light resources are 
##' \code{low}. PFG abundances higher than \code{LIGHT.thresh_medium} and lower
##' than this threshold imply \strong{medium amount of light}.
##' @param doDispersal default \code{FALSE}. If \code{TRUE}, seed dispersal is 
##' activated in the \code{FATE-HD} simulation, and associated parameters are 
##' required
##' @param doHabSuitability default \code{FALSE}. If \code{TRUE}, habitat  
##' suitability is activated in the \code{FATE-HD} simulation, and associated 
##' parameters are required
##' @param HABSUIT.ref_option an \code{integer} corresponding to the way of 
##' simulating the habitat suitability variation between years for each PFG, 
##' either random (1) or PFG specific (2)
##' @param doDisturbances default \code{FALSE}. If \code{TRUE}, disturbances 
##' are applied in the \code{FATE-HD} simulation, and associated parameters 
##' are required
##' @param DIST.no the number of disturbances
##' @param DIST.no_sub the number of way a PFG could react to a disturbance
##' @param DIST.freq the frequency of each disturbance (\emph{in years})
##' @param doSoil default \code{FALSE}. If \code{TRUE}, soil competition is
##' activated in the \code{FATE-HD} simulation, and associated parameters 
##' are required
##' @param doFire \emph{to be filled}
##' @param doDrought \emph{to be filled}
##' @param doHabStability \emph{to be filled}
##' @param doAliens \emph{to be filled}
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
##'     defined into the PFG SUCC files. \cr \cr
##'   }
##'   }
##'   \item{Simulation timing}{
##'   \itemize{
##'     \item \emph{required.simul_duration} : the duration of simulation 
##'     (\emph{in years})
##'     \item \emph{required.seeding_duration} : the duration of seeding 
##'     (\emph{in years})
##'     \item \emph{required.seeding_timestep} : the time interval at which 
##'     occurs the seeding, and until the seeding duration is not over 
##'     (\emph{in years})
##'     \item \emph{required.seeding_input} : the number of seeds dispersed
##'     for each PFG at each time step, and until the seeding duration is
##'     not over \cr \cr
##'   }
##'   }
##'   \item{To get abundances \cr per PFG per pixel}{
##'   \itemize{
##'     \item per strata
##'     \item for all strata
##'   }
##'   \deqn{abund = 10 000 * \frac{totalAbund}{\text{required.max_abund_[low/medium/high]}}}
##'   }
##' }
##' 
##' 
##' The other modules of \code{FATE-HD} can be activated within this file, and
##' if so, some additional parameters will be required :
##' 
##' \describe{
##'   \item{LIGHT}{= to influence plants fecundity and seed 
##'   recruitment according to PFG preferences for light conditions \cr
##'   = light resources are calculated as a proxy of PFG abundances
##'   within each height stratum \cr
##'   \itemize{
##'   \item{To transform PFG \cr abundances into \cr light resources}{
##'     \deqn{abund < \text{LIGHT.thresh_medium} \Leftrightarrow Light = High}
##'     \deqn{abund > \text{LIGHT.thresh_medium } \& \\
##'     abund < \text{LIGHT.thresh_low} \Leftrightarrow Light = Medium}
##'     \deqn{abund > \text{LIGHT.thresh_low} \Leftrightarrow Light = Low}
##'   }
##'   }
##'   \cr \cr
##'   }
##'   \item{DISPERSAL}{= to allow plants to disperse seeds according
##'   to 3 user-defined distances \cr
##'   \cr \cr
##'   }
##'   \item{HABITAT SUITABILITY}{= to influence plants fecundity and seed 
##'   recruitment according to PFG preferences for habitat conditions \cr
##'   = filter based 
##'   on maps given for each PFG within the \emph{Simul_parameters} file 
##'   with the \code{PFG_HAB_MASK} flag. \cr \cr
##'   These maps must contain values 
##'   between 0 and 1 corresponding to the probability of presence of the 
##'   group in each pixel. Each year (timestep), this value will be compared 
##'   to a reference value, and if superior, the PFG will be able to grow and 
##'   survive.
##'   \itemize{
##'     \item \emph{HABSUIT.ref_option} : the habitat suitability reference
##'     value can be set in two possible ways :
##'     \enumerate{
##'       \item \emph{random} : for each pixel, the reference value is drawn from
##'       a uniform distribution, and the same value is used for each PFG 
##'       within this pixel.
##'       \item \emph{PFG specific} : for each PFG, a mean value and a standard 
##'       deviation value are drawn from a uniform distribution. For each 
##'       pixel and for each PFG, the reference value is drawn from a normal 
##'       distribution of parameters the mean and standard deviation of the PFG.
##'        \cr \cr
##'     }
##'   }
##'   }
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
##'   \item{SOIL}{= to influence plants mortality according to PFG preferences 
##'   for soil conditions (fertility) \cr
##'   = soil composition is calculated as the weighted mean of each PFG's
##'   contribution \cr \cr
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
##'   \text{evenness} = - \frac{\Sigma(\text{PFG.proportion} * log(\text{PFG.proportion}))}{log(\text{no.PFG} - 1)}
##'   }
##'   \deqn{\text{with PFG.proportion} = \frac{\text{abund.PFG.in.Habitat}}{\text{abund.allPFG.in.Habitat}}}
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
##'   \item SIMULATION_DURATION
##'   \item SEEDING_DURATION
##'   \item SEEDING_TIMESTEP
##'   \item SEEDING_INPUT
##'   \item MAX_BY_COHORT
##'   \item MAX_ABUND_LOW
##'   \item MAX_ABUND_MEDIUM 
##'   \item MAX_ABUND_HIGH \cr \cr
##' }
##' 
##' If the simulation includes \emph{light competition} :
##' 
##' \itemize{
##'   \item DO_LIGHT_COMPETITION
##'   \item LIGHT_THRESH_MEDIUM
##'   \item LIGHT_THRESH_LOW
##' }
##' 
##' If the simulation includes \emph{dispersal} :
##' 
##' \itemize{
##'   \item DO_DISPERSAL
##' }
##' 
##' If the simulation includes \emph{habitat suitability} :
##' 
##' \itemize{
##'   \item DO_HAB_SUITABILITY
##'   \item HABSUIT_OPTION
##' }
##' 
##' If the simulation includes \emph{disturbances} :
##' 
##' \itemize{
##'   \item DO_DISTURBANCES
##'   \item NB_DISTURBANCES
##'   \item NB_SUBDISTURBANCES
##'   \item FREQ_DISTURBANCES
##' }
##' 
##' If the simulation includes \emph{soil competition} :
##' 
##' \itemize{
##'   \item DO_SOIL_COMPETITION
##' }
##' 
##' If the simulation includes \emph{drought disturbances} :
##' 
##' \itemize{
##'   \item DO_DROUGHT_DISTURBANCES
##'   \item NB_SUBDROUGHT
##'   \item CHRONO_POST_DROUGHT
##'   \item CHRONO_CURR_DROUGHT
##' }
##' 
##' If the simulation includes \emph{habitat stability check} :
##' 
##' \itemize{
##'   \item DO_HAB_STABILITY
##'   \item NB_HABITATS
##' }
##' 
##' If the simulation includes \emph{aliens introduction} :
##' 
##' \itemize{
##'   \item DO_ALIENS_DISTURBANCE
##'   \item FREQ_ALIENS
##' }
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' \code{\link{PRE_FATE.params_PFGdispersal}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}},
##' \code{\link{PRE_FATE.params_PFGsoil}}
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
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = 10
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_by_cohort = 5000000
##'                                  , required.max_abund_low = 3000000
##'                                  , required.max_abund_medium = 5000000
##'                                  , required.max_abund_high = 9000000
##'                                  , doLight = TRUE
##'                                  , LIGHT.thresh_medium = 13000000
##'                                  , LIGHT.thresh_low = 19000000
##'                                  , doDispersal = TRUE
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.ref_option = 1
##'                                  )
##'                                    
##' ## Create SEVERAL Global_parameters files
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 3
##'                                  , required.no_STRATA = 5
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = 10
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_by_cohort = 5000000
##'                                  , required.max_abund_low = 3000000
##'                                  , required.max_abund_medium = 5000000
##'                                  , required.max_abund_high = 9000000
##'                                  , doLight = TRUE
##'                                  , LIGHT.thresh_medium = 13000000
##'                                  , LIGHT.thresh_low = 19000000
##'                                  , doDispersal = TRUE
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.ref_option = c(1,2)
##'                                  )
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_globalParameters = function(
  name.simulation
  , opt.no_CPU = 1
  , opt.replacePrevious = FALSE
  , required.no_PFG
  , required.no_STRATA
  , required.simul_duration
  , required.seeding_duration
  , required.seeding_timestep
  , required.seeding_input
  , required.max_by_cohort
  , required.max_abund_low
  , required.max_abund_medium
  , required.max_abund_high
  , doLight = FALSE
  , LIGHT.thresh_medium
  , LIGHT.thresh_low
  , doDispersal = FALSE
  , doHabSuitability = FALSE
  , HABSUIT.ref_option
  , doDisturbances = FALSE
  , DIST.no
  , DIST.no_sub
  , DIST.freq
  , doSoil = FALSE
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
  
  .testParam_existFolder(name.simulation, "DATA/GLOBAL_PARAMETERS/")
  
  if (is.na(opt.no_CPU) ||
      is.null(opt.no_CPU) ||
      !is.numeric(opt.no_CPU) ||
      opt.no_CPU <= 0){
    warning(paste0("Wrong type of data!\n `opt.no_CPU` must be an integer > 0\n"
                   , " ==> Automatically set to 1"))
  }
  if (.testParam_notNum(required.no_PFG) ||
      required.no_PFG <= 0 ){
    .stopMessage_beInteger("required.no_PFG")
  }
  if (.testParam_notNum(required.no_STRATA) ||
      required.no_STRATA <= 0 ){
    .stopMessage_beInteger("required.no_STRATA")
  }
  if (.testParam_notNum(required.simul_duration) ||
      required.simul_duration <= 0 ) {
    .stopMessage_beInteger("required.simul_duration")
  }
  if (.testParam_notNum(required.seeding_duration) ||
      required.seeding_duration <= 0 ){
    .stopMessage_beInteger("required.seeding_duration")
  }
  if (.testParam_notNum(required.seeding_timestep) ||
      required.seeding_timestep <= 0 ){
    .stopMessage_beInteger("required.seeding_timestep")
  }
  if (.testParam_notNum(required.seeding_input) ||
      required.seeding_input <= 0 ){
    .stopMessage_beInteger("required.seeding_input")
  }
  if (.testParam_notNum(required.max_by_cohort) ||
      required.max_by_cohort <= 0 )
  {
    .stopMessage_beInteger("required.max_by_cohort")
  } else if (round(required.max_by_cohort) != required.max_by_cohort)
  {
    warning("`required.max_by_cohort` is a double. It will be converted (rounded) to an integer")
  }
  if (.testParam_notNum(required.max_abund_low) ||
      required.max_abund_low <= 0 )
  {
    .stopMessage_beInteger("required.max_abund_low")
  } else if (round(required.max_abund_low) != required.max_abund_low)
  {
    warning("`required.max_abund_low` is a double. It will be converted (rounded) to an integer")
  }
  if (.testParam_notNum(required.max_abund_medium) ||
      required.max_abund_medium <= 0 )
  {
    .stopMessage_beInteger("required.max_abund_medium")
  } else if (round(required.max_abund_medium) != required.max_abund_medium)
  {
    warning("`required.max_abund_medium` is a double. It will be converted (rounded) to an integer")
  }
  if (.testParam_notNum(required.max_abund_high) ||
      required.max_abund_high <= 0 )
  {
    .stopMessage_beInteger("required.max_abund_high")
  } else if (round(required.max_abund_high) != required.max_abund_high)
  {
    warning("`required.max_abund_high` is a double. It will be converted (rounded) to an integer")
  }
  
  if (doLight)
  {
    if (.testParam_notNum(LIGHT.thresh_medium) ||
        LIGHT.thresh_medium <= 0 )
    {
      .stopMessage_beInteger("LIGHT.thresh_medium")
    } else if (round(LIGHT.thresh_medium) != LIGHT.thresh_medium)
    {
      warning("`LIGHT.thresh_medium` is a double. It will be converted (rounded) to an integer")
    }
    if (.testParam_notNum(LIGHT.thresh_low) ||
        LIGHT.thresh_low <= 0 )
    {
      .stopMessage_beInteger("LIGHT.thresh_low")
    } else if (round(LIGHT.thresh_low) != LIGHT.thresh_low)
    {
      warning("`LIGHT.thresh_low` is a double. It will be converted (rounded) to an integer")
    }
  }
  
  if (doDispersal)
  {
    ## Nothing to do
  }
  
  if (doHabSuitability)
  {
    if (.testParam_notNum(HABSUIT.ref_option) ||
        !(HABSUIT.ref_option %in% c(1,2))){
      .stopMessage_content("HABSUIT.ref_option", c("1 (random)", "2 (distribution per PFG)"))
    }
  }
  
  if (doDisturbances)
  {
    if (.testParam_notNum(DIST.no) ||
        DIST.no <= 0 ){
      .stopMessage_beInteger("DIST.no")
    }
    if (.testParam_notNum(DIST.no_sub) ||
        DIST.no_sub <= 0 ){
      .stopMessage_beInteger("DIST.no_sub")
    }
    if (.testParam_notNum(DIST.freq) ||
        sum(DIST.freq <= 0) > 0){
      stop("Wrong type of data!\n `DIST.freq` must be a vector of integer > 0")
    } else if (length(DIST.freq) != DIST.no){
      stop("Wrong type of data!\n `DIST.freq` must contain as many values as the number of disturbances (`DIST.no`)")
    }
  }
  
  #################################################################################################
  
  if (doLight)
  {
    params.LIGHT = list(as.numeric(doLight)
                        , LIGHT.thresh_medium
                        , LIGHT.thresh_low)
    names.params.list.LIGHT = c("DO_LIGHT_COMPETITION"
                                , "LIGHT_THRESH_MEDIUM"
                                , "LIGHT_THRESH_LOW")
  } else
  {
    params.LIGHT = list(as.numeric(doLight))
    names.params.list.LIGHT = "DO_LIGHT_COMPETITION"
  }
  if (doDispersal)
  {
    params.DISP = list(as.numeric(doDispersal))
    names.params.list.DISP = c("DO_DISPERSAL")
  } else
  {
    params.DISP = list(as.numeric(doDispersal))
    names.params.list.DISP = "DO_DISPERSAL"
  }
  if (doHabSuitability)
  {
    params.HABSUIT = list(as.numeric(doHabSuitability)
                          , HABSUIT.ref_option)
    names.params.list.HABSUIT = c("DO_HAB_SUITABILITY"
                                  , "HABSUIT_OPTION")
  } else
  {
    params.HABSUIT = list(as.numeric(doHabSuitability))
    names.params.list.HABSUIT = "DO_HAB_SUITABILITY"
  }
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
    params.SOIL = list(as.numeric(doSoil))
    names.params.list.SOIL = c("DO_SOIL_COMPETITION")
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
                             , required.simul_duration
                             , required.seeding_duration
                             , required.seeding_timestep
                             , required.seeding_input
                             , required.max_by_cohort
                             , required.max_abund_low
                             , required.max_abund_medium
                             , required.max_abund_high
  )
  
  params.list = lapply(1:nrow(params.combi), function(x) {
    res = lapply(1:ncol(params.combi), function(y) { params.combi[x, y] })
    res = c(res, params.LIGHT)
    res = c(res, params.DISP)
    res = c(res, params.HABSUIT)
    res = c(res, params.DIST)
    res = c(res, params.SOIL)
    res = c(res, params.DROUGHT)
    res = c(res, params.HABSTAB)
    res = c(res, params.ALIEN)
  })
  
  no.start = 1
  if (!opt.replacePrevious)
  {
    previous.files = list.files(path = paste0(name.simulation, "/DATA/GLOBAL_PARAMETERS/")
                                , pattern = "^Global_parameters_")
    if (length(previous.files) > 0) {
      no.start = length(previous.files) + 1
    }
  }

  names.params.list = paste0("V", no.start:length(params.list))
  names.params.list.sub = c("NB_CPUS"
                            , "NB_FG"
                            , "NB_STRATUM"
                            , "SIMULATION_DURATION"
                            , "SEEDING_DURATION"
                            , "SEEDING_TIMESTEP"
                            , "SEEDING_INPUT"
                            , "MAX_BY_COHORT"
                            , "MAX_ABUND_LOW"
                            , "MAX_ABUND_MEDIUM"
                            , "MAX_ABUND_HIGH"
  )
  names.params.list.sub = c(names.params.list.sub, names.params.list.LIGHT)
  names.params.list.sub = c(names.params.list.sub, names.params.list.DISP)
  names.params.list.sub = c(names.params.list.sub, names.params.list.HABSUIT)
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
