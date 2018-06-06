### HEADER #####################################################################
##' @title Create \emph{Simul_parameters} parameter file for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_simulParameters
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create one (or several) parameter file
##' containing \code{PARAMETER FILENAMES} used in \code{FATE-HD} model.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' 
##' 
##' @details 
##' 
##' \describe{
##'   \item{To get abundances \cr per PFG per pixel}{
##'   }
##' }
##' 
##' 
##' @return A \code{.txt} file into the \code{name.simulation/PARAM_SIMUL/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item --NAMESPACE_CONSTANTS--
##'   \item --GLOBAL_PARAMS--
##'   \item --ARRAYS_SAVING_YEARS-- (\emph{optional})
##'   \item --OBJECTS_SAVING_YEARS-- (\emph{optional})
##'   \item --PFG_LIFE_HISTORY_PARAMS--
##'   \item --PFG_DISPERSAL_PARAMS--
##'   \item --PFG_DISTURBANCES_PARAMS-- (\emph{optional})
##'   \item --PFG_ENVSUIT-- (\emph{optional})
##'   \item --END_OF_FILE--
##' }
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
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
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.succ_option = "fateh"
##'                                  , required.no_PFG = 6
##'                                  , required.no_STRATA = 5
##'                                  , required.hs_option = 1
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_duration = c(10,50)
##'                                  , required.simul_duration = 100)
##' 
##' ## Create a SAVE_year_maps or/and SAVE_year_objects parameter file
##' PRE_FATE.params_saveYears(name.simulation = "FATE_simulation"
##'                           , years.maps = c(100, 150, 200)
##'                           , years.objects = 200)
##' 
##' ## Create a Changing_times parameter file
##' PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
##'                               , type.changing = "DIST"
##'                               , mat.changing = data.frame(year = c(50,50,80,80)
##'                                                           , order = c(1,2,1,2)
##'                                                           , file.name = c("MASK_DIST1_50.tif"
##'                                                                           , "MASK_DIST2_50.tif"
##'                                                                           , "MASK_DIST1_80.tif"
##'                                                                           , "MASK_DIST2_80.tif")))
##' 
##' ## Create PFG succession parameter files
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                           , type = c("C", "C", "H", "H", "P", "P")
##'                                                           , height = c(10, 250, 36, 68, 1250, 550)
##'                                                           , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                           , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                           , dispersal = 1
##'                                                           , light = c(4, 6, 3, 6, 5, 5)))
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
##'                              , mat.PFG.disp = data.frame(PFG = paste0("PFG", 1:6)
##'                                                          , d50 = rep(c(500, 500, 100),2)
##'                                                          , d99 = rep(c(10000, 15000, 20000),2)
##'                                                          , ldd = rep(c(100000, 50000, 100000),2)))
##' 
##' ## Create PFG disturbance parameter files
##' PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
##'                                , mat.PFG.dist = data.frame(name = rep(c("DIST1","DIST2"), each = 4)
##'                                                            , responseStage = rep(1:4, 2)
##'                                                            , KilledIndiv_H = c(0,0,0,0,1,1,0,0)
##'                                                            , KilledIndiv_C = c(0,10,10,10,1,1,0,0)
##'                                                            , KilledIndiv_P = c(10,10,10,10,10,0,0,0)
##'                                                            , ResproutIndiv_H = c(0,0,9,10,0,0,5,1)
##'                                                            , ResproutIndiv_C = c(0,0,0,0,0,0,5,1)
##'                                                            , ResproutIndiv_P = c(0,0,0,0,0,0,0,0)))
##' 
##' ## Create a Simulation parameter file
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
##'                                    
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_simulParameters = function(
  name.simulation
){
  
  if (missing(name.simulation) ||
      is.na(name.simulation) ||
      is.null(name.simulation) ||
      !is.character(name.simulation) ||
      !dir.exists(paste0(name.simulation, "/PARAM_SIMUL/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  }
  if (!dir.exists(paste0(name.simulation, "/DATA/NAMESPACE_CONSTANTS/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  }
  if (!dir.exists(paste0(name.simulation, "/DATA/GLOBAL_PARAMETERS/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  }
  if (!dir.exists(paste0(name.simulation, "/DATA/SAVE/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  }
  if (!dir.exists(paste0(name.simulation, "/DATA/PFGS/SUCC/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  }
  if (!dir.exists(paste0(name.simulation, "/DATA/PFGS/DISP/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  }
  if (!dir.exists(paste0(name.simulation, "/RESULTS/")))
  {
    stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  }
  
  #################################################################################################
  
  files.NAMESPACE = list.files(path = paste0(name.simulation, "/DATA/NAMESPACE_CONSTANTS")
                               , pattern = "^Namespace_constants.*.txt"
                               , full.names = TRUE)
  if (length(files.NAMESPACE) == 0)
  {
    stop(paste0("Wrong number of files!\n There is no adequate file "
                , "(`.txt` file starting with `Namespace_constants`) "
                , "into the DATA/NAMESPACE_CONSTANTS/ folder"))
  }
  
  #################################################################################################
  
  files.GLOBAL = list.files(path = paste0(name.simulation, "/DATA/GLOBAL_PARAMETERS")
                            , pattern = "^Global_parameters.*.txt"
                            , full.names = TRUE)
  if (length(files.GLOBAL) == 0)
  {
    stop(paste0("Wrong number of files!\n There is no adequate file "
                , "(`.txt` file starting with `Global_parameters`) "
                , "into the DATA/GLOBAL_PARAMETERS/ folder"))
  }
  
  #################################################################################################
  
  dirs.SAVE = list.dirs(path = paste0(name.simulation, "/DATA/SAVE")
                        , full.names = FALSE
                        , recursive = FALSE)
  if (length(dirs.SAVE) > 0)
  {
    dirs.SAVE = paste0(name.simulation, "/DATA/SAVE/", dirs.SAVE)
  } else
  {
    dirs.SAVE = paste0(name.simulation, "/DATA/SAVE")
  }
  
  #################################################################################################
  
  dirs.SCENARIO = list.dirs(path = paste0(name.simulation, "/DATA/SCENARIO")
                            , full.names = FALSE
                            , recursive = FALSE)
  
  if (length(dirs.SCENARIO) > 0)
  {
    dirs.SCENARIO = paste0(name.simulation, "/DATA/SCENARIO/", dirs.SAVE)
  } else
  {
    dirs.SCENARIO = paste0(name.simulation, "/DATA/SCENARIO")
  }
  dirs.SCENARIO.MASK = dirs.SCENARIO.HS = dirs.SCENARIO.DIST = vector()
  
  for (di in dirs.SCENARIO)
  {
    for (ty in c("MASK", "HS", "DIST"))
    {
      files.ty = list.files(path = di
                            , pattern = paste0("^", ty, "_changing_times")
                            , full.names = TRUE)
      if (length(files.ty) > 0)
      {
        eval(parse(text = paste0("dirs.SCENARIO.", ty, " = c(dirs.SCENARIO.", ty, ", di)")))
      }
    }
  }
  # } else
  # {
  #   dirs.SCENARIO.MASK = dirs.SCENARIO.HS = dirs.SCENARIO.DIST = paste0(name.simulation, "/DATA/SCENARIO")
  # }
  
  #################################################################################################
  #################################################################################################
  
  sce.mask = sce.hs = sce.dist = 0
  if (length(dirs.SCENARIO.MASK) > 0) sce.mask = 1:length(dirs.SCENARIO.MASK)
  if (length(dirs.SCENARIO.HS) > 0) sce.hs = 1:length(dirs.SCENARIO.HS)
  if (length(dirs.SCENARIO.DIST) > 0) sce.dist = 1:length(dirs.SCENARIO.DIST)
  
  PARAMS.combi = expand.grid(NAMESPACE = 1:length(files.NAMESPACE)
                             , GLOBAL = 1:length(files.GLOBAL)
                             , SAVE = 1:length(dirs.SAVE)
                             , SCENARIO.MASK = sce.mask
                             , SCENARIO.HS = sce.hs
                             , SCENARIO.DIST = sce.dist)

  #################################################################################################
  #################################################################################################
  
  for (i in 1:nrow(PARAMS.combi))
  {
    
    params.combi = data.frame(NAMESPACE = files.NAMESPACE[PARAMS.combi$NAMESPACE[i]]
                              , GLOBAL = files.GLOBAL[PARAMS.combi$GLOBAL[i]]
                              , SAVE.dir = paste0(name.simulation, "/DATA/RESULTS/SIMUL_V", i))
    
    names.params.combi = c("--NAMESPACE_CONSTANTS--"
                           , "--GLOBAL_PARAMS--"
                           , "--SAVE_DIR--")
    
    #################################################################################################
    
    di = dirs.SAVE[PARAMS.combi$SAVE[i]]
    
    ### -------------------------------------------------------------------- ###
    
    files.SAVE.maps = list.files(path = di
                                 , pattern = "^SAVE_YEARS_maps"
                                 , full.names = TRUE)
    if (length(files.SAVE.maps) == 0)
    {
      warning(paste0("There is no adequate file (`.txt` file starting with `SAVE_YEARS_maps`) "
                     , "into the folder ", di))
    } else if (length(files.SAVE.maps) == 1)
    {
      params.combi$SAVE.maps = files.SAVE.maps
      names.params.combi = c(names.params.combi, "--ARRAYS_SAVING_YEARS--")
    } else
    {
      stop(paste0("There is to many adequate files (`.txt` file starting with `SAVE_YEARS_maps`) "
                  , "into the folder", di))
    }
    
    ### -------------------------------------------------------------------- ###
    
    files.SAVE.objects = list.files(path = di
                                    , pattern = "^SAVE_YEARS_objects"
                                    , full.names = TRUE)
    if (length(files.SAVE.objects) == 0)
    {
      warning(paste0("There is no adequate file (`.txt` file starting with `SAVE_YEARS_objects`) "
                     , "into the folder ", di))
    } else if (length(files.SAVE.objects) == 1)
    {
      params.combi$SAVE.obj = files.SAVE.objects
      names.params.combi = c(names.params.combi, "--OBJECTS_SAVING_YEARS--")
    } else
    {
      stop(paste0("There is to many adequate files (`.txt` file starting with `SAVE_YEARS_objects`) "
                  , "into the folder", di))
    }
    #################################################################################################
    
    for (ty in c("MASK", "HS", "DIST"))
    {
      if (PARAMS.combi[, paste0("SCENARIO.", ty)][i] > 0)
      {
        eval(parse(text = paste0("di = dirs.SCENARIO.", ty, "[PARAMS.combi$SCENARIO.", ty, "[i]]")))
        
        files.SCENARIO.times = list.files(path = di
                                          , pattern = paste0("^", ty, "_changing_times")
                                          , full.names = TRUE)
        if (length(files.SCENARIO.times) == 0)
        {
          warning(paste0("There is no adequate file (`.txt` file starting with `", ty, "_changing_times`) "
                         , "into the folder ", di))
        } else if (length(files.SCENARIO.times) == 1)
        {
          eval(parse(text = paste0("params.combi$SCENARIO.", ty, " = files.SCENARIO.times")))
          if (ty == "HS"){
            names.params.combi = c(names.params.combi, paste0("--CLIM_CHANGE_TIME--"))
          } else {
            names.params.combi = c(names.params.combi, paste0("--", ty, "_CHANGE_TIME--"))
          }
        } else
        {
          stop(paste0("There is to many adequate files (`.txt` file starting with `", ty, "_changing_times`) "
                      , "into the folder", di))
        }
      }
    }
    
    ### -------------------------------------------------------------------- ###
    
    for (ty in c("MASK", "HS", "DIST"))
    {
      if (PARAMS.combi[, paste0("SCENARIO.", ty)][i] > 0)
      {
        eval(parse(text = paste0("di = dirs.SCENARIO.", ty, "[PARAMS.combi$SCENARIO.", ty, "[i]]")))
        files.SCENARIO.masks = list.files(path = di
                                          , pattern = paste0("^", ty, "_changing_masks")
                                          , full.names = TRUE)
        if (length(files.SCENARIO.masks) == 0)
        {
          warning(paste0("There is no adequate file (`.txt` file starting with `", ty, "_changing_masks`) "
                         , "into the folder ", di))
        } else if (length(files.SCENARIO.masks) > 0)
        {
          eval(parse(text = paste0("SCENARIO.", ty, " = files.SCENARIO.masks")))
        }
      }
    }
    
    #################################################################################################
    
    globi = as.character(files.GLOBAL[PARAMS.combi$GLOBAL[i]])
    
    ### -------------------------------------------------------------------- ###
    
    no_PFG = .getParam(params.lines = globi
                       , flag = "NB_FG"
                       , flag.split = " "
                       , is.num = TRUE)
    no_PFG = as.numeric(no_PFG)
    
    files.PFG.SUCC = list.files(path = paste0(name.simulation, "/DATA/PFGS/SUCC")
                                , pattern = "^SUCC"
                                , full.names = TRUE)
    if (length(files.PFG.SUCC) != no_PFG)
    {
      stop(paste0("There is not the same number of files "
                  , "(`.txt` file starting with `SUCC`) "
                  , "into the DATA/PFGS/SUCC/ folder as the number of PFG "
                  , "indicated into the file "
                  , globi))
    }
    
    files.PFG.DISP = list.files(path = paste0(name.simulation, "/DATA/PFGS/DISP")
                                , pattern = "^DISP"
                                , full.names = TRUE)
    if (length(files.PFG.DISP) != no_PFG)
    {
      stop(paste0("There is not the same number of files "
                  , "(`.txt` file starting with `DISP`) "
                  , "into the DATA/PFGS/DISP/ folder as the number of PFG "
                  , "indicated into the file "
                  , globi))
    }
    
    #################################################################################################
    
    doDisturbances = .getParam(params.lines = globi
                               , flag = "DO_DISTURBANCES"
                               , flag.split = " "
                               , is.num = TRUE)
    if (doDisturbances && !dir.exists(paste0(name.simulation, "/DATA/PFGS/DIST/")))
    {
      stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
    }
    files.PFG.DIST = list.files(path = paste0(name.simulation, "/DATA/PFGS/DIST")
                                , pattern = "^DIST"
                                , full.names = TRUE)
    if (length(files.PFG.DIST) != no_PFG)
    {
      warning(paste0("There is not the same number of files "
                     , "(`.txt` file starting with `DIST`) "
                     , "into the DATA/PFGS/DIST/ folder as the number of PFG "
                     , "indicated into the file "
                     , globi))
    }
    
    #################################################################################################
    
    succ_model = .getParam(params.lines = globi
                           , flag = "SUCC_MOD"
                           , flag.split = " "
                           , is.num = FALSE)
    if (succ_model == "fateh" && !dir.exists(paste0(name.simulation, "/DATA/PFGS/ENVSUIT/")))
    {
      stop("Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/PFGS/ENVSUIT/ folder")
    }
    files.PFG.HS = list.files(path = paste0(name.simulation, "/DATA/PFGS/ENVSUIT")
                              # , pattern = "^DIST"
                              , full.names = TRUE)
    if (length(files.PFG.HS) != no_PFG)
    {
      warning(paste0("There is not the same number of files "
                     , "into the DATA/PFGS/ENVSUIT/ folder as the number of PFG "
                     , "indicated into the file "
                     , globi))
    }
    
    
    #################################################################################################
    #################################################################################################    
    
    params.list = lapply(1:ncol(params.combi), function(x) { as.character(params.combi[, x]) })
    names.params.list = names.params.combi
    
    if (exists("SCENARIO.MASK"))
    {
      params.list = c(params.list, list(SCENARIO.MASK))
      names.params.list = c(names.params.list, "--MASK_CHANGE_MASK--")
    }
    if (exists("SCENARIO.HS"))
    {
      params.list = c(params.list, list(SCENARIO.HS))
      names.params.list = c(names.params.list, "--CLIM_CHANGE_MASK--")
    }
    if (exists("SCENARIO.DIST"))
    {
      params.list = c(params.list, list(SCENARIO.DIST))
      names.params.list = c(names.params.list, "--DIST_CHANGE_MASK--")
    }
    
    params.list = c(params.list, list(files.PFG.SUCC, files.PFG.DISP))
    names.params.list = c(names.params.list
                          , "--PFG_LIFE_HISTORY_PARAMS--"
                          , "--PFG_DISPERSAL_PARAMS--")
    
    if (doDisturbances){
      params.list = c(params.list, list(files.PFG.DIST))
      names.params.list = c(names.params.list
                            , "--PFG_DISTURBANCES_PARAMS--")
    }
    if (succ_model == "fateh"){
      params.list = c(params.list, list(files.PFG.HS))
      names.params.list = c(names.params.list
                            , "--PFG_ENVSUIT--")
    }
    
    params = c(params.list, list(""))
    names(params) = c(names.params.list, "--END_OF_FILE--")
    
    .createParams(params.file = paste0(name.simulation,
                                       "/PARAM_SIMUL/Simul_parameters_V",
                                       i,
                                       ".txt")
                  , params.list = params
                  , separator = "\n")
    
  }
}
