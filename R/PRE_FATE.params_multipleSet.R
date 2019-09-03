### HEADER #####################################################################
##' @title Create multiple set(s) of parameter files for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_multipleSet
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create multiple sets of parameters 
##' using Latin Hypercube Sampling to help find best combination of parameters 
##' (see \code{Details})
##'              
##' @param name.simulation.1 a \code{string} that corresponds to the main
##' directory or simulation name of the \code{FATE-HD} simulation from which to
##' retrieve the first parameter simulation file (\code{file.simulation.1}), 
##' and the second if given (\code{file.simulation.2}) and no other directory 
##' provided (\code{name.simulation.2 = NULL})
##' @param name.simulation.2 default \code{NULL} (\emph{optional}). \cr 
##' A \code{string} that corresponds to the main directory or simulation name 
##' of the \code{FATE-HD} simulation from which to retrieve the second 
##' parameter simulation file (\code{file.simulation.2})
##' @param file.simulation.1 a \code{string} that corresponds to the name of 
##' the simulation parameter file from which to retrieve parameter values that
##' will be used to build the multiple set of new parameters
##' @param file.simulation.2 default \code{NULL} (\emph{optional}). \cr 
##' A \code{string} that corresponds to the name of the second simulation 
##' parameter file from which to retrieve parameter values that will be used to 
##' build parameter ranges in comparison with values from 
##' \code{file.simulation.1}
##' @param no_simulations an \code{integer} corresponding to the number of set 
##' of parameters that will be produced according to Latin Hypercube Sampling 
##' (LHS)
##' @param opt.percent_max default \code{0.5}. Amount of variation (between 0 
##' and 1) around the original value of \code{MAX_BY_COHORT}, 
##' \code{MAX_ABUND_LOW}, \code{MAX_ABUND_MEDIUM}, \code{MAX_ABUND_HIGH} if 
##' selected
##' @param opt.percent_seeding default \code{0.5}. Amount of variation (between 0 
##' and 1) around the original value of \code{SEEDING_DURATION}, 
##' \code{SEEDING_TIMESTEP}, \code{SEEDING_INPUT} if selected
##' @param opt.percent_light default \code{0.5}. Amount of variation (between 0 
##' and 1) around the original value of \code{LIGHT_THRESH_MEDIUM}, 
##' \code{LIGHT_THRESH_LOW} if selected
##' @param do.max_by_cohort default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_BY_COHORT} parameter within \code{Global_parameters} file will be 
##' declined into a range of values
##' @param do.max_abund_low default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_ABUND_LOW} parameter within \code{Global_parameters} file will be 
##' declined into a range of values
##' @param do.max_abund_medium default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_ABUND_MEDIUM} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.max_abund_high default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_ABUND_HIGH} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.seeding_duration default \code{TRUE}. If \code{TRUE}, 
##' \code{SEEDING_DURATION} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.seeding_timestep default \code{TRUE}. If \code{TRUE}, 
##' \code{SEEDING_TIMESTEP} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.seeding_input default \code{TRUE}. If \code{TRUE}, 
##' \code{SEEDING_INPUT} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.no_STRATA default \code{TRUE}. If \code{TRUE}, 
##' \code{NB_STRATUM} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.LIGHT.thresh_medium default \code{TRUE}. If \code{TRUE}, 
##' \code{LIGHT_THRESH_MEDIUM} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.LIGHT.thresh_low default \code{TRUE}. If \code{TRUE}, 
##' \code{LIGHT_THRESH_LOW} parameter within \code{Global_parameters} file will 
##' be declined into a range of values
##' @param do.DISPERSAL.mode default \code{TRUE}. If \code{TRUE}, 
##' \code{DISPERSAL_MODE} parameter within \code{Global_parameters} file will 
##' be declined into its 3 possible values (either uniform kernel (1), 
##' exponential kernel (2) or exponential kernel with probability (3))
##' @param do.HABSUIT.ref_option default \code{TRUE}. If \code{TRUE}, 
##' \code{HABSUIT_OPTION} parameter within \code{Global_parameters} file will 
##' be declined into its 2 possible values (either random (1) or PFG specific 
##' (2))
##' 
##' 
##' 
##' @details 
##' 
##' A \code{FATE-HD} simulation requires several parameters to define 
##' general characteristics of the simulation : they are saved within a
##' \code{Global_parameters} file. To fit the model to a particular area 
##' and set of Plant Functional Groups (PFG), these are the parameters that 
##' should be optimized, since they are not data-dependant, unlike, for example, 
##' parameters related to PFG (height, maturity, dispersal distances, soil 
##' tolerance, etc).
##' 
##' (\emph{Note : this is true, except when varying the number of strata, which 
##' will have an impact on some parameters within SUCC and LIGHT PFG parameter 
##' files.})
##' 
##' \strong{The main idea is to start from a complete simulation folder, to 
##' select the parameters that should vary, and to create new parameter files 
##' with new parameter values based on pre-existing values. \cr}
##' 
##' Three possible scenarios are available :
##' \describe{
##'   \item{1 folder - \cr 1 simulation file}{
##'   \itemize{
##'     \item requested parameter values are extracted from the given 
##'     simulation file
##'     \item ranges are assigned to each parameter according to the specified 
##'     value \cr e.g. : if \code{opt.percent_seeding = 0.5}, and do.seeding_duration 
##'     is asked, values will be generated for this parameter between : 
##'     \deqn{SEEDING\_DURATION \pm SEEDING\_DURATION * \frac{50}{100}}
##'     \item according to the required number of parameter sets to be produced 
##'     (\code{no_simulations}), Latin Hypercube Sampling is applied to select 
##'     each new parameter values
##'     \item parameter files are created for these new parameter values
##'   }
##'   }
##'   \item{1 folder - \cr 2 simulation files}{
##'   \itemize{
##'     \item same as 1st scenario
##'     \item ranges assigned to each parameter correspond to the extracted 
##'     values (e.g. : if do.seeding_duration is asked, values will be generated 
##'     for this parameter between : 
##'     \code{SEEDING_DURATION (file_simulation.1)} and \code{SEEDING_DURATION 
##'     (file_simulation.2)}
##'   }
##'   }
##'   \item{2 folders - \cr 2 simulation files}{
##'   \itemize{
##'     \item same as 2nd scenario, except that the two given simulation files 
##'     come from two different simulation folders
##'   }
##'   }
##' }
##' 
##' \strong{Latin Hypercube Sampling} is a statistical method to generate a 
##' sampling of parameter values from a multidimensional space, while ensuring 
##' a good representation of the real variability.
##' The range of each parameter is known, and depending on the number of set of 
##' parameters asked to be obtained at the end, each range is more or less 
##' finely cut and values are drawn in order to explore the whole space of 
##' combinations.
##' 
##' 
##' 
##' 
##' @return A new folder containing the different sets of parameters asked. 
##' 
##' Depending on what elements have been asked to be varied, three types of 
##' files can have been modified :
##' \enumerate{
##'   \item the global parameter file
##'   \item the PFG succession files
##'   \item the PFG light succession files \cr \cr
##' }
##' 
##' Below are listed the parameters that can change (if selected) within each 
##' file :
##' 
##' \itemize{
##'   \item Into the \code{name.simulation/DATA/GLOBAL_PARAMETERS} folder :
##'   \itemize{
##'     \item NB_STRATUM
##'     \item SEEDING_DURATION
##'     \item SEEDING_TIMESTEP
##'     \item SEEDING_INPUT
##'     \item MAX_BY_COHORT
##'     \item MAX_ABUND_LOW
##'     \item MAX_ABUND_MEDIUM 
##'     \item MAX_ABUND_HIGH \cr \cr
##'   }
##'   If the simulation includes \emph{light competition} :
##'   \itemize{
##'     \item LIGHT_THRESH_MEDIUM
##'     \item LIGHT_THRESH_LOW
##'   }
##'   If the simulation includes \emph{dispersal} :
##'   \itemize{
##'     \item DISPERSAL_MODE
##'   }
##'   If the simulation includes \emph{habitat suitability} :
##'   \itemize{
##'     \item HABSUIT_OPTION
##'   }
##'   \item Into the \code{name.simulation/DATA/PFGS/SUCC} folder :
##'   \itemize{
##'     \item STRATA
##'     \item MAX_ABUNDANCE
##'     \item IMM_SIZE
##'     \item CHANG_STR_AGES
##'   }
##'   \item Into the \code{name.simulation/DATA/PFGS/LIGHT} folder :
##'   \itemize{
##'     \item SHADE_TOL
##'   }
##' }
##' 
##' 
##' @keywords FATE, simulation, Latin Hypercube Sampling
##' 
##' @seealso \code{\link[SPOT]{designLHD}},
##' \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' 
##' 
##' @examples
##' 
##' 
##' @export
##' 
##' @importFrom SPOT designLHD
##'
## END OF HEADER ###############################################################


PRE_FATE.params_multipleSet = function(
  name.simulation.1
  , name.simulation.2 = NULL
  , file.simulation.1
  , file.simulation.2 = NULL
  , no_simulations
  , opt.percent_max = 0.5
  , opt.percent_seeding = 0.5
  , opt.percent_light = 0.5
  , do.max_by_cohort = TRUE
  , do.max_abund_low = TRUE
  , do.max_abund_medium = TRUE
  , do.max_abund_high = TRUE
  , do.seeding_duration = TRUE
  , do.seeding_timestep = TRUE
  , do.seeding_input = TRUE
  , do.no_STRATA = TRUE
  , do.LIGHT.thresh_medium = TRUE
  , do.LIGHT.thresh_low = TRUE
  , do.DISPERSAL.mode = TRUE
  , do.HABSUIT.ref_option = TRUE
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
      sum(required.no_PFG <= 0) > 0 ){
    .stopMessage_beInteger("required.no_PFG")
  }
  if (.testParam_notNum(required.no_STRATA) ||
      sum(required.no_STRATA <= 0) > 0 ){
    .stopMessage_beInteger("required.no_STRATA")
  }
  if (.testParam_notNum(required.simul_duration) ||
      sum(required.simul_duration <= 0) > 0 ){
    .stopMessage_beInteger("required.simul_duration")
  }
  if (.testParam_notNum(required.seeding_duration) ||
      sum(required.seeding_duration <= 0) > 0 ){
    .stopMessage_beInteger("required.seeding_duration")
  }
  if (.testParam_notNum(required.seeding_timestep) ||
      sum(required.seeding_timestep <= 0) > 0 ){
    .stopMessage_beInteger("required.seeding_timestep")
  }
  if (.testParam_notNum(required.seeding_input) ||
      sum(required.seeding_input <= 0) > 0 ){
    .stopMessage_beInteger("required.seeding_input")
  }
  if (.testParam_notNum(required.max_by_cohort) ||
      sum(required.max_by_cohort <= 0) > 0 )
  {
    .stopMessage_beInteger("required.max_by_cohort")
  } else if (round(required.max_by_cohort) != required.max_by_cohort)
  {
    warning("`required.max_by_cohort` is a double. It will be converted (rounded) to an integer")
  }
  if (.testParam_notNum(required.max_abund_low) ||
      sum(required.max_abund_low <= 0) > 0 )
  {
    .stopMessage_beInteger("required.max_abund_low")
  } else if (round(required.max_abund_low) != required.max_abund_low)
  {
    warning("`required.max_abund_low` is a double. It will be converted (rounded) to an integer")
  }
  if (.testParam_notNum(required.max_abund_medium) ||
      sum(required.max_abund_medium <= 0) > 0 )
  {
    .stopMessage_beInteger("required.max_abund_medium")
  } else if (round(required.max_abund_medium) != required.max_abund_medium)
  {
    warning("`required.max_abund_medium` is a double. It will be converted (rounded) to an integer")
  }
  if (.testParam_notNum(required.max_abund_high) ||
      sum(required.max_abund_high <= 0) > 0 )
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
    if (.testParam_notNum(DISPERSAL.mode) ||
        sum(!(DISPERSAL.mode %in% c(1,2,3))) > 0){
      .stopMessage_content("DISPERSAL.mode", c("1 (uniform kernel)"
                                               , "2 (exponential kernel)"
                                               , "3 (exponential kernel with probability)"))
    }
  }
  
  if (doHabSuitability)
  {
    if (.testParam_notNum(HABSUIT.ref_option) ||
        sum(!(HABSUIT.ref_option %in% c(1,2))) > 0){
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
                             , as.integer(required.max_by_cohort)
                             , as.integer(required.max_abund_low)
                             , as.integer(required.max_abund_medium)
                             , as.integer(required.max_abund_high)
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
