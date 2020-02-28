### HEADER #####################################################################
##' @title Replace parameter value(s) from a parameter file
##' 
##' @name .setParam
##'
##' @author Maya Guéguen
##' 
##' @description This function finds in a text file the value(s) of a given 
##' parameter, and replace it with new value(s).
##' 
##' @param params.lines a \code{string} that corresponds to the name of the 
##' file from which to extract the parameter value
##' @param flag a \code{string} that corresponds to the parameter name to be 
##' extracted and that must be present into the \code{param.lines} file
##' @param flag.split either "\code{ }" or "\code{^--.*--$}", depending on the 
##' type of parameter file
##' @param value a \code{string} or a \code{numeric} value (it can also be a 
##' \code{vector}) containing the new value of the parameter to be changed
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
##' readLines("FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt")
##'                                  
##' ## Change number of PFG
##' .setParam(params.lines = "FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt"
##'           , flag = "NB_FG"
##'           , flag.split = " "
##'           , value = 14)
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
##' ## Change names of PFG succession files
##' readLines("FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt")
##' 
##' ## GIVING ERROR : not the same number of elements
##' # .setParam(params.lines = "FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt"
##' #           , flag = "PFG_LIFE_HISTORY_PARAMS"
##' #           , flag.split = "^--.*--$"
##' #           , value = c("toto", "tata", "titi"))
##' 
##' .setParam(params.lines = "FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt"
##'           , flag = "PFG_LIFE_HISTORY_PARAMS"
##'           , flag.split = "^--.*--$"
##'           , value = rep(c("toto", "tata", "titi"), 8))
##'           
##' readLines("FATE_PNE/PARAM_SIMUL/Simul_parameters_V1.txt")
##' 
##' 
##' @export
##'
## END OF HEADER ###############################################################


.setParam = function(params.lines
                     , flag
                     , flag.split
                     , value
){
  
  if (.testParam_notChar(params.lines))
  {
    .stopMessage_beChar("params.lines")
  } else
  {
    .testParam_existFile(params.lines)
  }
  if (.testParam_notChar(flag) ||
      nchar(flag) == 0)
  {
    .stopMessage_beChar("flag")
  }
  if (missing(flag.split) ||
      is.na(flag.split) ||
      !is.character(flag.split) ||
      !(flag.split %in% c(" ", "^--.*--$")))
  {
    .stopMessage_content("flag.split", c(" ", "^--.*--$"))
  }
  if (.testParam_notDef(value) || sum(nchar(value) == 0) > 0)
  {
    stop("No data given!\n (missing `value` information)")
  }
  
  param.name = params.lines
  params.lines = readLines(params.lines)
  if (flag.split == "^--.*--$")
  {
    if (length(grep("--END_OF_FILE--", params.lines)) == 0)
    {
      stop(paste0("Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (", param.name, ")"))
    }
  }
  if (length(grep(flag.split, params.lines)) <= ifelse(flag.split == "^--.*--$", 1, 0)){
    stop(paste0("Wrong type of data!\n `flag.split` (", flag.split, ") is not found within `params.lines` (", param.name, ")"))
  }
  if (length(grep(flag, params.lines)) == 0){
    stop(paste0("Wrong type of data!\n `flag` (", flag, ") is not found within `params.lines` (", param.name, ")"))
  }
  
  if(flag.split == " "){
    value.line = grep(flag, params.lines, value = TRUE)
    params.lines = sub(value.line
                       , paste(flag, paste0(value, collapse = flag.split), sep = flag.split)
                       , params.lines)
  } else {
    ind.flag.split = grep(flag.split, params.lines)
    ind.flag = grep(paste0("--", flag, "--"), params.lines)
    if (length(ind.flag) == 0)
    {
      stop(paste0("Wrong type of data!\n `flag` (", flag, ") is not found within `params.lines` (", param.name, ")"))
    }
    ind.start = which(ind.flag.split == ind.flag)
    if (ind.flag.split[ind.start + 1] == ind.start + 1)
    {
      stop(paste0("Wrong type of data!\n `flag` (", flag, ") does not contain any value"))
    }
    
    ind1 = (ind.flag.split[ind.start] + 1)
    ind2 = ifelse(length(ind.flag.split) == 1
                  , max(length(params.lines), ind1)
                  , ind.flag.split[ind.start + 1] - 1)
    if (length(ind1:ind2) == length(value)){
      params.lines[ind1:ind2] = value
    } else {
      stop(paste0("Wrong dimension(s) of data!\n `value` does not have the same number of elements ("
                  , length(value), ") than `flag` (", flag, ", ", length(ind1:ind2), ")"))
    }
  }
  cat(params.lines, sep = "\n", file = param.name, append = FALSE)
  message(paste0("\n The parameter file ", param.name, " has been successfully corrected !\n"))
}

