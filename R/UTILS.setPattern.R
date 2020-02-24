### HEADER #####################################################################
##' @title Replace a pattern with a new within all parameter files of a 
##' \code{FATE-HD} simulation folder
##' 
##' @name .setPattern
##'
##' @author Maya Guéguen
##' 
##' @description This function scans all the files within a \code{FATE-HD} 
##' simulation folder to find a specific pattern and replace it with a new one
##' 
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param opt.name.file (\emph{optional}) \cr a \code{string} that corresponds 
##' to the name of the file (or part) in which to search and change the pattern
##' @param pattern.tofind a \code{string} that corresponds to the pattern to 
##' find
##' @param pattern.toreplace a \code{string} that corresponds to the pattern to 
##' replace
##' 
##' 
##' @examples 
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 6
##'                                  , required.no_STRATA = 5
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = c(10,50)
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_abund_low = 30000
##'                                  , required.max_abund_medium = 50000
##'                                  , required.max_abund_high = 90000)
##'                                  
##'                                  
##' ## Change number of PFG
##' readLines("FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt")
##' 
##' .setPattern(name.simul = "FATE_simulation"
##'             , opt.name.file = "Global_parameters_V1.txt"
##'             , pattern.tofind = "NB_FG 6"
##'             , pattern.toreplace = "NB_FG 14")
##'           
##' readLines("FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt")
##' 
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##'                                 
##' ## Load example data
##' PNE_PARAM = .loadData("PNE_PARAM")
##' 
##' ## PNE_PARAM$succ_light : data.frame
##' ## PNE_PARAM$strata_limits : vector
##' ## PNE_PARAM$disp : data.frame
##' ## PNE_PARAM$dist : data.frame
##' ## PNE_PARAM$global : vector
##' 
##' ## Create a skeleton folder
##' PRE_FATE.skeletonDirectory(name.simulation = "FATE_PNE")
##' 
##' ## Create PFG succession parameter files : predefined of strata limits
##' tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity")]
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_PNE"
##'                               , mat.PFG.succ = tab
##'                               , strata.limits = PNE_PARAM$strata_limits
##'                               , strata.limits_reduce = FALSE)
##' 
##' ## Create PFG light parameter files : predefined of strata limits
##' tab = PNE_PARAM$succ_light[, c("PFG", "type", "height", "maturity", "longevity", "light")]
##' PRE_FATE.params_PFGlight(name.simulation = "FATE_PNE"
##'                          , mat.PFG.succ = tab
##'                          , strata.limits = PNE_PARAM$strata_limits
##'                          , strata.limits_reduce = FALSE)
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_PNE"
##'                              , mat.PFG.disp = PNE_PARAM$disp)
##' 
##' ## Create PFG disturbance parameter files
##' PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_PNE"
##'                                , mat.PFG.dist = PNE_PARAM$dist)
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_PNE"
##'                                  , required.no_PFG = PNE_PARAM$global["NB_FG"]
##'                                  , required.no_STRATA = PNE_PARAM$global["NB_STRATUM"]
##'                                  , required.simul_duration = PNE_PARAM$global["SIMULATION_DURATION"]
##'                                  , required.seeding_duration = PNE_PARAM$global["SEEDING_DURATION"]
##'                                  , required.seeding_timestep = PNE_PARAM$global["SEEDING_TIMESTEP"]
##'                                  , required.seeding_input = PNE_PARAM$global["SEEDING_INPUT"]
##'                                  , required.max_abund_low = PNE_PARAM$global["MAX_ABUND_LOW"]
##'                                  , required.max_abund_medium = PNE_PARAM$global["MAX_ABUND_MEDIUM"]
##'                                  , required.max_abund_high = PNE_PARAM$global["MAX_ABUND_HIGH"]
##'                                  , doLight = TRUE
##'                                  , LIGHT.thresh_medium = PNE_PARAM$global["LIGHT.thresh_medium"]
##'                                  , LIGHT.thresh_low = PNE_PARAM$global["LIGHT.thresh_low"]
##'                                  , doDispersal = TRUE
##'                                  , DISPERSAL.mode = PNE_PARAM$global["DISPERSAL.mode"]
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.ref_option = PNE_PARAM$global["HABSUIT.ref_option"]
##'                                  , doDisturbances = TRUE
##'                                  , DIST.no = PNE_PARAM$global["DIST.no"]
##'                                  , DIST.no_sub = PNE_PARAM$global["DIST.no_sub"]
##'                                  , DIST.freq = rep(PNE_PARAM$global["DIST.freq"]
##'                                                    , PNE_PARAM$global["DIST.no"])
##' )
##' 
##' ## Create simulation masks
##' library(raster)
##' writeRaster(PNE_PARAM$masks$maskEcrins
##'             , file = "FATE_PNE/DATA/MASK/mask.tif"
##'             , overwrite = TRUE)
##' writeRaster(PNE_PARAM$masks$noDisturb
##'             , file = "FATE_PNE/DATA/MASK/noDisturb.tif"
##'             , overwrite = TRUE)
##' 
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_PNE"
##'                                 , name.mask = "mask.tif"
##'                                 , name.dist = "noDisturb.tif")
##'
##' 
##' ## Change value of POTENTIAL_FECUNDITY in all PFG succession files
##' readLines("FATE_PNE/DATA/PFGS/SUCC/SUCC_C1_ruderal.txt")
##' 
##' .setPattern(name.simul = "FATE_PNE"
##'             , opt.name.file = "SUCC_"
##'             , pattern.tofind = "POTENTIAL_FECUNDITY 100"
##'             , pattern.toreplace = "POTENTIAL_FECUNDITY 500")
##' 
##' readLines("FATE_PNE/DATA/PFGS/SUCC/SUCC_C1_ruderal.txt")
##' readLines("FATE_PNE/DATA/PFGS/SUCC/SUCC_P5_alpext.txt")
##' 
##' 
##' ## Change name of simulation folder in all files
##' readLines("FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt")
##' 
##' .setPattern(name.simul = "FATE_PNE"
##'             , pattern.tofind = "^FATE_PNE/"
##'             , pattern.toreplace = "FATE_BIGSIMUL/")
##' 
##' readLines("FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt")
##' 
##' 
##' @export
##'
## END OF HEADER ###############################################################


.setPattern = function(name.simulation
                       , opt.name.file = NULL
                       , pattern.tofind
                       , pattern.toreplace
                       
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  if (.testParam_notChar(pattern.tofind))
  {
    .stopMessage_beChar("pattern.tofind") 
  }
  if (.testParam_notChar(pattern.toreplace))
  {
    .stopMessage_beChar("pattern.toreplace") 
  }
  
  all.files = list.files(path = name.simulation
                         , pattern = ".txt$"
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    stop(paste0("Missing data!\n The folder ", name.simulation, " does not contain adequate files (.txt)"))
  }
  
  if (is.null(opt.name.file) ||
      (!is.null(opt.name.file) && !is.character(opt.name.file)) ||
      (!is.null(opt.name.file) && nchar(opt.name.file) == 0)){
    warning("As `opt.name.file` does not contain character value, it will be ignored")
  } else {
    all.files = all.files[grep(opt.name.file, all.files)]
    if (length(all.files) == 0){
      stop(paste0("Missing data!\n The folder ", name.simulation, " does not contain adequate files (", opt.name.file, ")"))
    }
  }
  
  for (fi in all.files)
  {
    params.lines = readLines(con = fi, warn = FALSE)
    if (length(grep(pattern.tofind, params.lines)) > 0){
      params.lines = sub(pattern.tofind, pattern.toreplace, params.lines)
      cat(params.lines, sep = "\n", file = fi, append = FALSE)
      message(paste0("\n The parameter file ", fi, " has been successfully corrected !\n"))
    }
  }
}

