### HEADER #####################################################################
##' @title Create \emph{Global_parameters} parameter file for a \code{FATE}
##' simulation
##' 
##' @name PRE_FATE.params_globalParameters
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter file(s) 
##' containing \code{GLOBAL PARAMETERS} used in \code{FATE} model.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr an \code{integer} 
##' corresponding to the number of resources that can be used to parallelize 
##' the \code{FATE} simulation
##' @param opt.replacePrevious (\emph{optional}) default \code{FALSE}. \cr 
##' If \code{TRUE}, pre-existing files inside 
##' \code{name.simulation/DATA/GLOBAL_PARAMETERS} folder will be replaced
##' @param required.no_PFG an \code{integer} corresponding to the number of PFG
##' @param required.no_strata an \code{integer} corresponding to the number of 
##' height strata
##' @param required.simul_duration an \code{integer} corresponding to the 
##' duration of simulation (\emph{in years})
##' @param required.seeding_duration an \code{integer} corresponding to the 
##' duration of seeding (\emph{in years})
##' @param required.seeding_timestep an \code{integer} corresponding to the 
##' time interval at which occurs the seeding, and until the seeding duration 
##' is not over (\emph{in years})
##' @param required.seeding_input an \code{integer} corresponding to the number 
##' of seeds attributed to each PFG at each time step, and until the seeding 
##' duration is not over
##' @param required.max_abund_low an \code{integer} in the order of 
##' \code{1 000} to rescale abundance values of small PFG
##' @param required.max_abund_medium an \code{integer} in the order of 
##' \code{1 000} to rescale abundance values of intermediate PFG
##' @param required.max_abund_high an \code{integer} in the order of 
##' \code{1 000} to rescale abundance values of tall PFG
##' @param doLight default \code{FALSE}.\cr If \code{TRUE}, light competition 
##' is activated in the \code{FATE} simulation, and associated parameters are 
##' required
##' @param LIGHT.thresh_medium (\emph{optional}) \cr an \code{integer} in the 
##' order of \code{1 000} to convert PFG abundances in each stratum into light 
##' resources. It corresponds to the limit of abundances above which light 
##' resources are \code{medium}. PFG abundances lower than this threshold imply 
##' \strong{high amount of light}. It is consequently lower than 
##' \code{LIGHT.thresh_low}.
##' @param LIGHT.thresh_low (\emph{optional}) \cr an \code{integer} in the order 
##' of \code{1 000} to convert PFG abundances in each strata into light 
##' resources. It corresponds to the limit of abundances above which light 
##' resources are \code{low}. PFG abundances higher than 
##' \code{LIGHT.thresh_medium} and lower than this threshold imply 
##' \strong{medium amount of light}.
##' @param doSoil default \code{FALSE}. \cr If \code{TRUE}, soil competition is 
##' activated in the \code{FATE} simulation, and associated parameters 
##' are required
##' @param SOIL.init (\emph{optional}) \cr a \code{double} corresponding to the 
##' soil value to initialize all pixels when starting the \code{FATE} 
##' simulation
##' @param SOIL.retention (\emph{optional}) \cr a \code{double} corresponding 
##' to the percentage of soil value of the previous simulation year that will 
##' be kept in the calculation of the soil value of the current simulation year
##' @param doDispersal default \code{FALSE}. \cr If \code{TRUE}, seed dispersal 
##' is activated in the \code{FATE} simulation, and associated parameters are 
##' required
##' @param DISPERSAL.mode (\emph{optional}) \cr an \code{integer} corresponding 
##' to the way of simulating the seed dispersal for each PFG, either packets 
##' kernel (\code{1}), exponential kernel (\code{2}) or exponential kernel with 
##' probability (\code{3})
##' @param doHabSuitability default \code{FALSE}. \cr If \code{TRUE}, habitat 
##' suitability is activated in the \code{FATE} simulation, and associated 
##' parameters are required
##' @param HABSUIT.ref_option (\emph{optional}) \cr an \code{integer} 
##' corresponding to the way of simulating the habitat suitability variation 
##' between years for each PFG, either random (\code{1}) or PFG specific 
##' (\code{2})
##' @param doDisturbances default \code{FALSE}. \cr If \code{TRUE}, disturbances 
##' are applied in the \code{FATE} simulation, and associated parameters are 
##' required
##' @param DIST.no (\emph{optional}) \cr an \code{integer} corresponding to the 
##' number of disturbances
##' @param DIST.no_sub (\emph{optional}) \cr an \code{integer} corresponding to 
##' the number of way a PFG could react to a disturbance
##' @param DIST.freq (\emph{optional}) \cr a \code{vector} of \code{integer} 
##' corresponding to the frequency of each disturbance (\emph{in years})
##' @param doDrought default \code{FALSE}. \cr If \code{TRUE}, drought 
##' disturbances are applied in the \code{FATE} simulation, and associated 
##' parameters are required
##' @param DROUGHT.no_sub (\emph{optional}) \cr an \code{integer} corresponding 
##' to the number of way a PFG could react to a drought disturbance
##' @param doAliens default \code{FALSE}. \cr If \code{TRUE}, invasive plant 
##' introduction is activated in the \code{FATE} simulation, and associated 
##' parameters are required
##' @param ALIEN.no (\emph{optional}) \cr an \code{integer} corresponding to the 
##' number of introductions
##' @param ALIEN.freq (\emph{optional}) \cr a \code{vector} of \code{integer} 
##' corresponding to the frequency of each introduction (\emph{in years})
##' @param doFire \emph{to be filled}
##' 
##' 
##' 
##' @details 
##' 
##' The \strong{core module} of \code{FATE} requires several parameters to 
##' define general characteristics of the simulation :
##' 
##' \describe{
##'   \item{Studied system}{ \cr
##'   \describe{
##'     \item{no_PFG}{the number of plant functional groups that will be 
##'     included into the simulation. \cr This number should match with the 
##'     number of files that will be given to parameterize the different 
##'     activated modules with the characteristics of each group (\file{SUCC}, 
##'     \file{DISP}, ...).}
##'     \item{no_STRATA}{the number of height strata that will be used into the 
##'     succession module. \cr This number should match with the maximum number 
##'     of strata possible defined into the PFG \file{SUCC} files.}
##'   }
##'   }
##'   \item{Simulation timing}{ \cr
##'   \describe{
##'     \item{simul_duration}{the duration of simulation (\emph{in years})}
##'     \item{seeding_duration}{the duration of seeding (\emph{in years})}
##'     \item{seeding_timestep}{the time interval at which occurs the seeding, 
##'     and until the seeding duration is not over (\emph{in years})}
##'     \item{seeding_input}{the number of seeds dispersed for each PFG at each 
##'     time step, and until the seeding duration is not over \cr \cr}
##'   }
##'   }
##' }
##' 
##' 
##' The \strong{other modules} of \code{FATE} can be activated within this 
##' file, and if so, some additional parameters will be required :
##' 
##' \describe{
##'   \item{LIGHT}{= to influence seed recruitment and plant mortality according 
##'   to PFG preferences for light conditions \cr (see 
##'   \code{\link{PRE_FATE.params_PFGlight}})\cr
##'   = light resources are calculated as a proxy of PFG abundances within each 
##'   height stratum \cr \cr
##'   To transform PFG abundances into light resources :
##'   \deqn{abund_{\text{ PFG}_{all}\text{, }\text{Stratum}_k} < 
##'   \text{LIGHT.thresh_medium} \;\; \Leftrightarrow \;\; 
##'   light_{\text{ Stratum}_k} = \text{High}}
##'   
##'   \deqn{\text{LIGHT.thresh_medium } < 
##'   abund_{\text{ PFG}_{all}\text{, }\text{Stratum}_k} < 
##'   \text{LIGHT.thresh_low} \\ \Leftrightarrow \;\; 
##'   light_{\text{ Stratum}_k} = \text{Medium}}
##'   
##'   \deqn{abund_{\text{ PFG}_{all}\text{, }\text{Stratum}_k} > 
##'   \text{LIGHT.thresh_low} \;\; \Leftrightarrow \;\; 
##'   light_{\text{ Stratum}_k} = \text{Low}}
##'   \emph{As light resources are directly obtained from PFG abundances, 
##'   \code{LIGHT.thresh_medium} and \code{LIGHT.thresh_low} parameters should 
##'   be on the same scale than \code{required.max_abund_low}, 
##'   \code{required.max_abund_medium} and \code{required.max_abund_high} 
##'   parameters from the core module.} \cr \cr
##'   }
##'   \item{SOIL}{= to influence seed recruitment and plant mortality 
##'   according to PFG preferences for soil conditions \cr (see 
##'   \code{\link{PRE_FATE.params_PFGsoil}}) \cr
##'   = soil composition is calculated as the weighted mean of each PFG's 
##'   contribution with a possible retention of the soil value of the previous 
##'   simulation year 
##'   \deqn{Soil_y + \text{SOIL.retention} * (Soil_{y-1} - Soil_y)}
##'   with
##'   \deqn{Soil_y =  \sum abund_{\text{ PFG}_i\text{, }y} * 
##'   \text{contrib}_{\text{ PFG}_i}}
##'   \cr \cr
##'   }
##'   \item{DISPERSAL}{= to allow plants to disperse seeds according to 3 
##'   user-defined distances \cr (see \code{\link{PRE_FATE.params_PFGdispersal}})
##'   \cr \cr Three modes of dispersal (\code{DISPERSAL.mode}) are available :
##'     \enumerate{
##'       \item \emph{packets kernel} :
##'       \itemize{
##'         \item homogeneous dispersal of 50\% of the seeds within the 
##'         \code{d50} circle
##'         \item dispersal of 49\% of the seeds within the \code{d99 - d50} 
##'         ring with the same concentration as in the first circle but by pairs 
##'         of pixel (see \emph{Boulangeat et al, 2014})
##'         \item dispersal of 1\% of the seeds within the \code{ldd - d99} ring 
##'         into one random pixel
##'       }
##'       \item \emph{exponential kernel} : seeds are dispersed within each 
##'       concentric circle (\code{d50}, \code{d99} and \code{ldd}) according to 
##'       a decreasing exponential density law (lambda = 1)
##'       \item \emph{exponential kernel with probability} : seeds are dispersed 
##'       within each concentric circle (\code{d50}, \code{d99} and \code{ldd}) 
##'       according to a decreasing exponential density law (lambda = 1) and a 
##'       continuous decreasing probability with distance \cr \cr
##'     }
##'   }
##'   \item{HABITAT SUITABILITY}{= to influence plants fecundity and seed 
##'   recruitment according to PFG preferences for habitat conditions \cr
##'   = filter based on maps given for each PFG within the 
##'   \emph{Simul_parameters} file with the \code{PFG_HAB_MASK} flag \cr (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}) \cr \cr
##'   These maps must contain values between \code{0} and \code{1} corresponding 
##'   to the probability of presence of the PFG in each pixel. Each year 
##'   (timestep), this value will be compared to a reference value, and if 
##'   superior, the PFG will be able to grow and survive. \cr
##'   Two methods to define this habitat suitability reference value are 
##'   available (\code{HABSUIT.ref_option}) :
##'     \enumerate{
##'       \item \emph{random} : for each pixel, the reference value is drawn 
##'       from a uniform distribution, and the same value is used for each PFG 
##'       within this pixel.
##'       \item \emph{PFG specific} : for each PFG, a mean value and a 
##'       standard deviation value are drawn from a uniform distribution. For 
##'       each pixel and for each PFG, the reference value is drawn from a 
##'       normal distribution of parameters the mean and standard deviation of 
##'       the PFG. \cr \cr
##'     }
##'   }
##'   \item{DISTURBANCES}{= to influence plant mortality and / or resprouting 
##'   according to PFG tolerances to these events \cr (see 
##'   \code{\link{PRE_FATE.params_PFGdisturbance}})\cr
##'   = defined for events such as mowing, grazing, but also urbanization, 
##'   crops, etc \cr
##'   = filter based on maps given for each disturbance within the 
##'   \emph{Simul_parameters} file with the \code{DIST_MASK} flag \cr (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}) \cr \cr
##'   These maps, containing either \code{0} or \code{1}, define the impact zone 
##'   of each perturbation, and the user will have to define how each PFG will 
##'   be impacted depending on age and life stage. 
##'   \describe{
##'     \item{DIST.no}{the number of different disturbances}
##'     \item{DIST.no_sub}{the number of way a PFG could react to a 
##'     perturbation}
##'     \item{DIST.freq}{the frequency of each disturbance 
##'     (\emph{in years}) \cr \cr}
##'   }
##'   }
##'   \item{DROUGHT}{= to experience extreme events with a direct and a 
##'   delayed response on PFG \cr
##'   = based on a map given within the \emph{Simul_parameters} file with the 
##'   \code{DROUGHT_MASK} flag \cr (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}) \cr \cr
##'   This map must contain values representing proxies for drought intensity, 
##'   like moisture values, in the sense that the lower the values, the higher 
##'   the chance of experiencing a drought event. Developed canopy closure helps 
##'   to reduce these values. The intensity of the drought event (moderate or 
##'   severe) is determined based on thresholds defined for each PFG according 
##'   to, for example, their moisture preference, as well as the number of 
##'   cumulated consecutive years during which the PFG experienced a drought 
##'   (see \code{\link{PRE_FATE.params_PFGdrought}}).
##'   \describe{
##'     \item{no drought}{if \eqn{di_y > \text{threshold.MOD}_{\text{ PFG}_i}}, 
##'     the counter of cumulated consecutive years of drought experienced by the 
##'     PFG will decrease : \deqn{\text{counter}_{\text{ PFG}_i} = 
##'     \text{counter}_{\text{ PFG}_i} - 
##'     \text{counter.RECOVERY}_{\text{ PFG}_i}}}
##'     \item{moderate drought}{
##'     \itemize{
##'       \item if \eqn{\text{threshold.SEV}_{\text{ PFG}_i} < di_y < 
##'       \text{threshold.MOD}_{\text{ PFG}_i}}
##'       \item if \eqn{di_y < \text{threshold.SEV}_{\text{ PFG}_i} \;\; 
##'       \text{  &  } \;\; \text{counter}_{\text{ PFG}_i} = 0}
##'     }
##'     then fecundity and recruitment are set to \code{0} for this year, and 
##'     counter is incremented : \eqn{\text{counter}_{\text{ PFG}_i} ++}
##'     }
##'     \item{severe drought}{
##'     \itemize{
##'       \item if \eqn{di_y < \text{threshold.SEV}_{\text{ PFG}_i} \;\; 
##'       \text{  &  } \;\; \text{counter.SENS}_{\text{ PFG}_i} \leq 
##'       \text{counter}_{\text{ PFG}_i} < \text{counter.CUM}_{\text{ PFG}_i}}
##'       \item if \eqn{\text{counter}_{\text{ PFG}_i} \geq 
##'       \text{counter.CUM}_{\text{ PFG}_i}}
##'     }
##'     then PFG experiences \code{immediate} drought-related mortality ; 
##'     and the year after, fecundity and recruitment will be set to \code{0} 
##'     and PFG will experience \code{delayed} drought-related mortality. \cr \cr
##'     }
##'   }
##'   As for the disturbances module, the user will have to define how each PFG 
##'   will be impacted depending on age and life stage. 
##'   \describe{
##'     \item{(\emph{DROUGHT.no})}{\emph{!not required!} \cr = 2, the 
##'     \code{immediate} and \code{delayed} responses}
##'     \item{DROUGHT.no_sub}{the number of way a PFG could react to each of 
##'     these two perturbations}
##'     \item{(\emph{DROUGHT.freq})}{\emph{!not required!} \cr the map of 
##'     drought intensity proxy defined within the \emph{Simul_parameters} file 
##'     with the \code{DROUGHT_MASK} flag, as well as the 
##'     \code{DROUGHT_CHANGEMASK_YEARS} and \code{DROUGHT_CHANGEMASK_FILES} 
##'     flags, make it possible to manage the frequency and the variation of 
##'     drought values (see \code{\link{PRE_FATE.params_simulParameters}})
##'     \cr \cr}
##'   }
##'   }
##'   \item{INVASIVE \cr INTRODUCTION}{= to add new PFG during the simulation \cr
##'   = defined for events such as invasive introduction, colonization, but also 
##'   new crops development, reintroduction, etc \cr
##'   = filter based on maps given for each PFG within the 
##'   \emph{Simul_parameters} file with the \code{PFG_MASK_ALIENS} flag \cr (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}) \cr \cr
##'   These maps, containing either \code{0} or \code{1}, define the 
##'   introduction areas. \cr If the habitat suitability filter is on, 
##'   suitability maps will also be needed for these new groups.
##'   \describe{
##'     \item{ALIEN.no}{the number of different introductions}
##'     \item{ALIEN.freq}{the frequency of each introduction (\emph{in years})}
##'   }
##'   }
##' }
##' 
##' 
##' @return A \code{.txt} file into the 
##' \code{name.simulation/DATA/GLOBAL_PARAMETERS} directory with the following 
##' parameters :
##' 
##' \itemize{
##'   \item NO_CPU
##'   \item NO_PFG
##'   \item NO_STRATA
##'   \item SIMULATION_DURATION
##'   \item SEEDING_DURATION
##'   \item SEEDING_TIMESTEP
##'   \item SEEDING_INPUT
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
##' If the simulation includes \emph{soil competition} :
##' 
##' \itemize{
##'   \item DO_SOIL_COMPETITION
##'   \item SOIL_INIT
##'   \item SOIL_RETENTION
##' }
##' 
##' If the simulation includes \emph{dispersal} :
##' 
##' \itemize{
##'   \item DO_DISPERSAL
##'   \item DISPERSAL_MODE
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
##'   \item DIST_NO
##'   \item DIST_NOSUB
##'   \item DIST_FREQ
##' }
##'  
##' If the simulation includes \emph{drought disturbance} :
##' 
##' \itemize{
##'   \item DO_DROUGHT_DISTURBANCE
##'   \item DROUGHT_NOSUB
##' }
##' 
##' If the simulation includes \emph{aliens introduction} :
##' 
##' \itemize{
##'   \item DO_ALIENS_INTRODUCTION
##'   \item ALIENS_NO
##'   \item ALIENS_FREQ
##' }
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' \code{\link{PRE_FATE.params_PFGsoil}},
##' \code{\link{PRE_FATE.params_PFGdispersal}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}},
##' \code{\link{PRE_FATE.params_PFGdrought}},
##' \code{\link{PRE_FATE.params_simulParameters}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 3
##'                                  , required.no_strata = 5
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = 10
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_abund_low = 30000
##'                                  , required.max_abund_medium = 50000
##'                                  , required.max_abund_high = 90000
##'                                  , doLight = TRUE
##'                                  , LIGHT.thresh_medium = 130000
##'                                  , LIGHT.thresh_low = 190000
##'                                  , doDispersal = TRUE
##'                                  , DISPERSAL.mode = 1
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.ref_option = 1
##'                                  )
##'                                    
##' ## Create SEVERAL Global_parameters files
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 3
##'                                  , required.no_strata = 5
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = 10
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_abund_low = 30000
##'                                  , required.max_abund_medium = 50000
##'                                  , required.max_abund_high = 90000
##'                                  , doLight = TRUE
##'                                  , LIGHT.thresh_medium = 130000
##'                                  , LIGHT.thresh_low = 190000
##'                                  , doDispersal = TRUE
##'                                  , DISPERSAL.mode = 1
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.ref_option = c(1,2)
##'                                  )
##' 
##' 
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##'                  
##' ## Load example data
##'                                  
##' 
##' @export
##' 
## END OF HEADER ###############################################################


PRE_FATE.params_globalParameters = function(
  name.simulation
  , opt.no_CPU = 1
  , opt.replacePrevious = FALSE
  , required.no_PFG
  , required.no_strata
  , required.simul_duration
  , required.seeding_duration
  , required.seeding_timestep
  , required.seeding_input
  , required.max_abund_low
  , required.max_abund_medium
  , required.max_abund_high
  , doLight = FALSE
  , LIGHT.thresh_medium
  , LIGHT.thresh_low
  , doSoil = FALSE
  , SOIL.init
  , SOIL.retention
  , doDispersal = FALSE
  , DISPERSAL.mode
  , doHabSuitability = FALSE
  , HABSUIT.ref_option
  , doDisturbances = FALSE
  , DIST.no
  , DIST.no_sub
  , DIST.freq
  , doDrought = FALSE
  , DROUGHT.no_sub
  , doAliens = FALSE
  , ALIEN.no
  , ALIEN.freq
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
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/GLOBAL_PARAMETERS/")
  
  if (is.na(opt.no_CPU) ||
      is.null(opt.no_CPU) ||
      !is.numeric(opt.no_CPU) ||
      opt.no_CPU <= 0){
    warning(paste0("Wrong type of data!\n `opt.no_CPU` must be an integer > 0\n"
                   , " ==> Automatically set to 1"))
  }
  .testParam_notInteger.m("required.no_PFG", required.no_PFG)
  .testParam_notInteger.m("required.no_strata", required.no_strata)
  .testParam_notInteger.m("required.simul_duration", required.simul_duration)
  .testParam_notInteger.m("required.seeding_duration", required.seeding_duration)
  .testParam_notInteger.m("required.seeding_timestep", required.seeding_timestep)
  .testParam_notInteger.m("required.seeding_input", required.seeding_input)
  .testParam_notInteger.m("required.max_abund_low", required.max_abund_low)
  .testParam_notRound.m("required.max_abund_low", required.max_abund_low)
  .testParam_notInteger.m("required.max_abund_medium", required.max_abund_medium)
  .testParam_notRound.m("required.max_abund_medium", required.max_abund_medium)
  .testParam_notInteger.m("required.max_abund_high", required.max_abund_high)
  .testParam_notRound.m("required.max_abund_high", required.max_abund_high)
  if (sum(required.max_abund_low > required.max_abund_medium) > 0)
  {
    stop(paste0("Wrong type of data!\n `required.max_abund_low` must contain "
                , "values equal or inferior to `required.max_abund_medium`"))
  }
  if (sum(required.max_abund_medium > required.max_abund_high) > 0)
  {
    stop(paste0("Wrong type of data!\n `required.max_abund_medium` must contain "
                , "values equal or inferior to `required.max_abund_high`"))
  }
  if (doLight)
  {
    .testParam_notInteger.m("LIGHT.thresh_medium", LIGHT.thresh_medium)
    .testParam_notRound.m("LIGHT.thresh_medium", LIGHT.thresh_medium)
    .testParam_notInteger.m("LIGHT.thresh_low", LIGHT.thresh_low)
    .testParam_notRound.m("LIGHT.thresh_low", LIGHT.thresh_low)
    if (sum(LIGHT.thresh_medium > LIGHT.thresh_low) > 0){
      stop(paste0("Wrong type of data!\n `LIGHT.thresh_medium` must contain "
                  , "values equal or inferior to `LIGHT.thresh_low`"))
    }
  }
  if (doSoil)
  {
    .testParam_notNum.m("SOIL.init", SOIL.init)
    .testParam_notBetween.m("SOIL.retention", SOIL.retention, 0, 1)
  }
  if (doDispersal)
  {
    .testParam_notInValues.m("DISPERSAL.mode", DISPERSAL.mode, c(1, 2, 3))
  }
  if (doHabSuitability)
  {
    .testParam_notInValues.m("HABSUIT.ref_option", HABSUIT.ref_option, c(1, 2))
  }
  if (doDisturbances)
  {
    .testParam_notInteger.m("DIST.no", DIST.no)
    .testParam_notInteger.m("DIST.no_sub", DIST.no_sub)
    .testParam_notInteger.m("DIST.freq", DIST.freq)
    if (length(DIST.freq) != DIST.no){
      stop(paste0("Wrong type of data!\n `DIST.freq` must contain as many "
                  , "values as the number of disturbances (`DIST.no`)"))
    }
  }
  if (doDrought)
  {
    .testParam_notInteger.m("DROUGHT.no_sub", DROUGHT.no_sub)
  }
  if (doAliens)
  {
    .testParam_notInteger.m("ALIEN.no", ALIEN.no)
    .testParam_notInteger.m("ALIEN.freq", ALIEN.freq)
    if (length(ALIEN.freq) != ALIEN.no){
      stop(paste0("Wrong type of data!\n `ALIEN.freq` must contain as many "
                  , "values as the number of introductions (`ALIEN.no`)"))
    }
  }
  
  #############################################################################
  
  if (doLight)
  {
    params.LIGHT = list(as.numeric(doLight)
                        , as.integer(LIGHT.thresh_medium)
                        , as.integer(LIGHT.thresh_low))
    names.params.list.LIGHT = c("DO_LIGHT_COMPETITION"
                                , "LIGHT_THRESH_MEDIUM"
                                , "LIGHT_THRESH_LOW")
  } else
  {
    params.LIGHT = list(as.numeric(doLight))
    names.params.list.LIGHT = "DO_LIGHT_COMPETITION"
  }
  if (doSoil)
  {
    params.SOIL = list(as.numeric(doSoil)
                       , as.integer(SOIL.init)
                       , as.integer(SOIL.retention))
    names.params.list.SOIL = c("DO_SOIL_COMPETITION"
                               , "SOIL_INIT"
                               , "SOIL_RETENTION")
  } else
  {
    params.SOIL = list(as.numeric(doSoil))
    names.params.list.SOIL = "DO_SOIL_COMPETITION"
  }
  if (doDispersal)
  {
    params.DISP = list(as.numeric(doDispersal)
                       , DISPERSAL.mode)
    names.params.list.DISP = c("DO_DISPERSAL"
                               , "DISPERSAL_MODE")
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
                               , "DIST_NO"
                               , "DIST_NOSUB"
                               , "DIST_FREQ")
  } else
  {
    params.DIST = list(as.numeric(doDisturbances))
    names.params.list.DIST = "DO_DISTURBANCES"
  }
  if (doDrought)
  {
    params.DROUGHT = list(as.numeric(doDrought)
                          , DROUGHT.no_sub)
    names.params.list.DROUGHT = c("DO_DROUGHT_DISTURBANCE"
                                  , "DROUGHT_NOSUB")
  } else
  {
    params.DROUGHT = list(as.numeric(doDrought))
    names.params.list.DROUGHT = "DO_DROUGHT_DISTURBANCE"
  }
  if (doAliens)
  {
    params.ALIEN = list(as.numeric(doAliens)
                        , ALIEN.no
                        , ALIEN.freq)
    names.params.list.ALIEN = c("DO_ALIENS_INTRODUCTION"
                                , "ALIENS_NO"
                                , "ALIENS_FREQ")
  } else
  {
    params.ALIEN = list(as.numeric(doAliens))
    names.params.list.ALIEN = "DO_ALIENS_INTRODUCTION"
  } 
  
  #############################################################################
  
  params.combi = expand.grid(opt.no_CPU
                             , required.no_PFG
                             , required.no_strata
                             , required.simul_duration
                             , required.seeding_duration
                             , required.seeding_timestep
                             , required.seeding_input
                             , as.integer(required.max_abund_low)
                             , as.integer(required.max_abund_medium)
                             , as.integer(required.max_abund_high)
  )
  
  params.list = lapply(1:nrow(params.combi), function(x) {
    res = lapply(1:ncol(params.combi), function(y) { params.combi[x, y] })
    res = c(res, params.LIGHT)
    res = c(res, params.SOIL)
    res = c(res, params.DISP)
    res = c(res, params.HABSUIT)
    res = c(res, params.DIST)
    res = c(res, params.DROUGHT)
    res = c(res, params.ALIEN)
  })
  
  no.start = 1
  if (!opt.replacePrevious)
  {
    previous.files = list.files(path = paste0(name.simulation
                                              , "/DATA/GLOBAL_PARAMETERS/")
                                , pattern = "^Global_parameters_")
    if (length(previous.files) > 0) {
      no.start = length(previous.files) + 1
    }
  }
  
  names.params.list = paste0("V", no.start:length(params.list))
  names.params.list.sub = c("NO_CPU"
                            , "NO_PFG"
                            , "NO_STRATA"
                            , "SIMULATION_DURATION"
                            , "SEEDING_DURATION"
                            , "SEEDING_TIMESTEP"
                            , "SEEDING_INPUT"
                            , "MAX_ABUND_LOW"
                            , "MAX_ABUND_MEDIUM"
                            , "MAX_ABUND_HIGH"
  )
  names.params.list.sub = c(names.params.list.sub, names.params.list.LIGHT)
  names.params.list.sub = c(names.params.list.sub, names.params.list.SOIL)
  names.params.list.sub = c(names.params.list.sub, names.params.list.DISP)
  names.params.list.sub = c(names.params.list.sub, names.params.list.HABSUIT)
  names.params.list.sub = c(names.params.list.sub, names.params.list.DIST)
  names.params.list.sub = c(names.params.list.sub, names.params.list.DROUGHT)
  names.params.list.sub = c(names.params.list.sub, names.params.list.ALIEN)
  
  
  for (i in 1:length(params.list)){
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation
                                       , "/DATA/GLOBAL_PARAMETERS/Global_parameters_"
                                       , names.params.list[i]
                                       , ".txt")
                  , params.list = params)
  }
  
}
