### HEADER #####################################################################
##' @title Create \emph{Simul_parameters} parameter file for a \code{FATE-HD}
##' simulation
##' 
##' @name PRE_FATE.params_simulParameters
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create one (or several) parameter 
##' file containing \code{PARAMETER FILENAMES} used in \code{FATE-HD} model.
##'              
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE-HD} simulation
##' @param name.mask a \code{string} that corresponds to the file name of a 
##' raster mask, with either 0 or 1 within each pixel, 1 corresponding to the 
##' cells of the studied area in which the succession module of the 
##' \code{FATE-HD} simulation will take place
##' @param name.dist (\emph{optional}) \cr a \code{string} that corresponds 
##' to the file name of a raster mask, with either 0 or 1 within each pixel, 
##' 1 corresponding to the cells of the studied area in which the succession 
##' module of the \code{FATE-HD} simulation will take place
##' 
##' 
##' 
##' @details 
##' 
##' The \code{FATE-HD} software takes only one input parameter : a file containing
##' links to other files containing all the parameters and data needed by the 
##' program to run.
##' 
##' 
##' \describe{
##'   \item{GLOBAL_PARAMS}{file where parameters related to the simulation 
##'   definition are referred (e.g. number of PFG involved, number of height 
##'   strata, simulation duration, computer resources, modules loaded, ...) \cr
##'   (see \code{\link{PRE_FATE.params_globalParameters}}) \cr \cr
##'   }
##'   \item{SAVE_DIR}{directory where simulation outputs will be stored}
##'   \item{ARRAYS_SAVING_ \cr YEARS (\emph{optional})}{file containing the 
##'   years for which simulation maps will be saved \cr
##'   (see \code{\link{PRE_FATE.params_saveYears}})
##'   }
##'   \item{OBJECTS_SAVING_ \cr YEARS (\emph{optional})}{file containing the 
##'   years for which simulation outputs will be saved \cr
##'   (see \code{\link{PRE_FATE.params_saveYears}}) \cr \cr
##'   }
##'   \item{MASK}{raster mask that will define the study area}
##'   \item{MASK_CHANGE_TIME \cr (\emph{optional})}{file containing the years to 
##'   change rasters for the succession module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{MASK_CHANGE_MASK \cr (\emph{optional})}{file containing the files to 
##'   change rasters for the succession module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_LIFE_HISTORY_ \cr PARAMS}{PFG life history related parameters 
##'   (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGsuccession}}) \cr \cr
##'   }
##'   \item{PFG_DISPERSAL_ \cr PARAMS (\emph{optional})}{PFG dispersal capacity related parameters 
##'   (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdispersal}}) \cr \cr
##'   }
##'   \item{PFG_HAB_MASK \cr (\emph{optional})}{raster masks (one by PFG) containing 
##'   PFG habitat suitability for the study area}
##'   \item{HAB_CHANGE_TIME \cr (\emph{optional})}{file containing the years to 
##'   change rasters for the habitat suitability module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{HAB_CHANGE_MASK \cr (\emph{optional})}{file containing the files to 
##'   change rasters for the habitat suitability module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_DISTURBANCES_ \cr PARAMS (\emph{optional})}{PFG disturbance 
##'   related parameters in terms of resprouting and mortality (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdisturbance}})
##'   }
##'   \item{DIST_MASK \cr (\emph{optional})}{raster masks that will define the 
##'   disturbance areas}
##'   \item{DIST_CHANGE_TIME \cr (\emph{optional})}{file containing the years to 
##'   change rasters for the disturbance module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{DIST_CHANGE_MASK \cr (\emph{optional})}{file containing the files to 
##'   change rasters for the disturbance module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{PFG_SOIL_ \cr PARAMS (\emph{optional})}{PFG soil contribution and tolerance
##'    related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGsoil}}) \cr \cr
##'   }
##' }
##' 
##' 
##' 
##' @note 
##' 
##' \itemize{
##'   \item The links to files are \strong{NOT absolute paths}. \cr
##'   \emph{When relative paths are used}, the user should be careful of the
##'   folder from which the simulation is launched. \cr
##'   \emph{A function allows to transform these relative paths into absolute
##'   ones. (!not available yet!)}
##'   \item \strong{The order of files matters!} \cr
##'   For instance the first link below \code{--PFG_LIFE_HISTORY_PARAMS--} flag 
##'   has to match (the same PFG) with the first item below the 
##'   \code{--PFG_DISPERSAL_PARAMS--} flag.
##' }
##' 
##' 
##' 
##' 
##' @return A \code{.txt} file into the \code{name.simulation/PARAM_SIMUL/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item \strong{--GLOBAL_PARAMS--}
##'   \item \strong{--SAVE_DIR--}
##'   \item --ARRAYS_SAVING_YEARS-- (\emph{optional})
##'   \item --OBJECTS_SAVING_YEARS-- (\emph{optional})
##'   \item \strong{--MASK--}
##'   \item --MASK_CHANGE_TIME-- (\emph{optional})
##'   \item --MASK_CHANGE_MASK-- (\emph{optional})
##'   \item \strong{--PFG_LIFE_HISTORY_PARAMS--}
##'   \item --PFG_DISPERSAL_PARAMS-- (\emph{optional})
##'   \item --PFG_HAB_MASK-- (\emph{optional})
##'   \item --HAB_CHANGE_TIME-- (\emph{optional})
##'   \item --HAB_CHANGE_MASK-- (\emph{optional})
##'   \item --PFG_DISTURBANCES_PARAMS-- (\emph{optional})
##'   \item --DIST_MASK-- (\emph{optional})
##'   \item --DIST_CHANGE_TIME-- (\emph{optional})
##'   \item --DIST_CHANGE_MASK-- (\emph{optional})
##'   \item --PFG_SOIL_PARAMS-- (\emph{optional})
##'   \item --END_OF_FILE--
##' }
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' \code{\link{PRE_FATE.params_PFGdispersal}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}},
##' \code{\link{PRE_FATE.params_PFGsoil}},
##' \code{\link{PRE_FATE.params_saveYears}},
##' \code{\link{PRE_FATE.params_changingYears}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
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
##'                                  , required.max_by_cohort = 5000000
##'                                  , required.max_abund_low = 3000000
##'                                  , required.max_abund_medium = 5000000
##'                                  , required.max_abund_high = 9000000)
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
##'                                                           , longevity = c(12, 200, 25, 4, 110, 70)))
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
##'                             , mat.PFG.disp = data.frame(PFG = paste0("PFG", 1:6)
##'                                                         , MODE = 1
##'                                                         , d50 = rep(c(50, 50, 10),2)
##'                                                         , d99 = rep(c(1000, 1500, 2000),2)
##'                                                         , ldd = rep(c(10000, 5000, 10000), 2)))
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
##' ## Create PFG soil parameter files
##' PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
##'                         , mat.PFG.soil = data.frame(PFG = paste0("PFG", 1:6)
##'                                                     , type = c("C", "C", "H", "H", "P", "P")
##'                                                     , soil_contrib = c(2.5, 3, 4.8, 2.5, 3, 4.8)
##'                                                     , soil_tol_min = c(2, 3, 3, 2, 3, 3)
##'                                                     , soil_tol_max = c(3, 3, 6, 3, 3, 6)))
##' 
##' ## Create a Simulation parameter file
##' file.create("FATE_simulation/DATA/MASK/mask.tif")
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
##'                                 , name.mask = "mask.tif")
##' 
##' for (pfg in 1:6){
##'   file.create(paste0("FATE_simulation/DATA/PFGS/ENVSUIT/mask_PFG", pfg, ".tif"))
##' }
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
##'                                 , name.mask = "mask.tif")
##'                                    
##' 
##' @export
##' 
##' @importFrom foreach foreach
##'
## END OF HEADER ###############################################################

# setwd("/Users/gueguen/Documents/PACKAGES/RFate/data_supplements/Bauges/")
# name.simulation = "FATE_Bauges_TEST"
# name.mask = "MASK_100m.tif"
# 
# PRE_FATE.params_simulParameters(name.simulation = name.simulation
#                                 , name.mask = name.mask
#                                 , name.dist = name.mask)

PRE_FATE.params_simulParameters = function(
  name.simulation
  , name.mask
  , name.dist = NULL
){
  
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "DATA/GLOBAL_PARAMETERS/")
  .testParam_existFolder(name.simulation, "DATA/SAVE/")
  .testParam_existFolder(name.simulation, "DATA/SCENARIO/")
  .testParam_existFolder(name.simulation, "DATA/MASK/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/SUCC/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/DISP/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/HABSUIT/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/LIGHT/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/SOIL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  if (.testParam_notChar(name.mask))
  {
    .stopMessage_beChar("name.mask") 
  } else
  {
    .testParam_existFile(paste0(name.simulation, "/DATA/MASK/", name.mask))
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
    dirs.SCENARIO = paste0(name.simulation, "/DATA/SCENARIO/", dirs.SCENARIO)
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
      } else
      {
        warning(paste0("There is no adequate file (`.txt` file starting with `", ty, "_changing_times`) "
                       , "into the folder ", di))
      }
    }
  }
  
  #################################################################################################
  
  dirs.HAB = list.dirs(path = paste0(name.simulation, "/DATA/PFGS/HABSUIT")
                       , full.names = FALSE
                       , recursive = FALSE)
  if (length(dirs.HAB) > 0)
  {
    dirs.HAB = paste0(name.simulation, "/DATA/PFGS/HABSUIT/", dirs.HAB)
  } else
  {
    dirs.HAB = paste0(name.simulation, "/DATA/PFGS/HABSUIT")
  }
  
  dirs.HABSUIT = vector()
  for (di in dirs.HAB)
  {
    files.hs = list.files(path = di
                          # , pattern = paste0("^", ty, "_changing_times")
                          , full.names = TRUE)
    if (length(files.hs) > 0)
    {
      dirs.HABSUIT = c(dirs.HABSUIT, di)
    }
  }
  
  #################################################################################################
  #################################################################################################
  
  sce.mask = sce.hs = sce.dist = ras.hs = 0
  if (length(dirs.SCENARIO.MASK) > 0) sce.mask = 1:length(dirs.SCENARIO.MASK)
  if (length(dirs.SCENARIO.HS) > 0) sce.hs = 1:length(dirs.SCENARIO.HS)
  if (length(dirs.SCENARIO.DIST) > 0) sce.dist = 1:length(dirs.SCENARIO.DIST)
  if (length(dirs.HABSUIT) > 0) ras.hs = 1:length(dirs.HABSUIT)
  
  PARAMS.combi = expand.grid(GLOBAL = 1:length(files.GLOBAL)
                             , SAVE = 1:length(dirs.SAVE)
                             , SCENARIO.MASK = sce.mask
                             , SCENARIO.HS = sce.hs
                             , SCENARIO.DIST = sce.dist
                             , PFG.HABSUIT = ras.hs)
  
  #################################################################################################
  #################################################################################################
  
  for (i in 1:nrow(PARAMS.combi))
  {
    
    params.combi = data.frame(GLOBAL = files.GLOBAL[PARAMS.combi$GLOBAL[i]]
                              , MASK = paste0(name.simulation, "/DATA/MASK/", name.mask))
    
    names.params.combi = c("--GLOBAL_PARAMS--"
                           , "--MASK--")
    
    #################################################################################################
    
    di = dirs.SAVE[PARAMS.combi$SAVE[i]]
    
    ### -------------------------------------------------------------------- ###
    
    di.opt = data.frame(pat = c("SAVE_YEARS_maps", "SAVE_YEARS_objects")
                        , nam = c("SAVE.maps", "SAVE.obj")
                        , param = c("ARRAYS_SAVING_YEARS", "OBJECTS_SAVING_YEARS"))
    
    for (ii in 1:nrow(di.opt))
    {
      files.found = list.files(path = di
                               , pattern = paste0("^", di.opt$pat[ii])
                               , full.names = TRUE)
      if (length(files.found) == 0)
      {
        warning(paste0("There is no adequate file (`.txt` file starting with `"
                       , di.opt$pat[ii], "`) "
                       , "into the folder ", di))
      } else if (length(files.found) == 1)
      {
        eval(parse(text = paste0("params.combi$", di.opt$nam[ii], " = files.found")))
        names.params.combi = c(names.params.combi, paste0("--", di.opt$param[ii], "--"))
      } else
      {
        stop(paste0("There is too many adequate files (`.txt` file starting with `"
                    , di.opt$pat[ii], "`) "
                    , "into the folder ", di))
      }
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
        # if (length(files.SCENARIO.times) == 0)
        # {
        #   warning(paste0("There is no adequate file (`.txt` file starting with `", ty, "_changing_times`) "
        #                  , "into the folder ", di))
        # } else 
        if (length(files.SCENARIO.times) == 1)
        {
          eval(parse(text = paste0("params.combi$SCENARIO.", ty, " = files.SCENARIO.times")))
          if (ty == "HS"){
            names.params.combi = c(names.params.combi, paste0("--HAB_CHANGE_TIME--"))
          } else {
            names.params.combi = c(names.params.combi, paste0("--", ty, "_CHANGE_TIME--"))
          }
        } else
        {
          stop(paste0("There is too many adequate files (`.txt` file starting with `", ty, "_changing_times`) "
                      , "into the folder ", di))
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
    
    do.SUCC = 1
    do.DISP = .getParam(params.lines = globi
                        , flag = "DO_DISPERSAL"
                        , flag.split = " "
                        , is.num = TRUE)
    do.DIST = .getParam(params.lines = globi
                        , flag = "DO_DISTURBANCES"
                        , flag.split = " "
                        , is.num = TRUE)
    do.LIGHT = .getParam(params.lines = globi
                         , flag = "DO_LIGHT_COMPETITION"
                         , flag.split = " "
                         , is.num = TRUE)
    do.SOIL = .getParam(params.lines = globi
                        , flag = "DO_SOIL_COMPETITION"
                        , flag.split = " "
                        , is.num = TRUE)
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("SUCC", "DISP", "LIGHT", "SOIL", "DIST")
    
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_existFolder(name.simulation, paste0("DATA/PFGS/", mod, "/"))
        
        ## Get folders
        dirs.mod = list.dirs(path = paste0(name.simulation, "/DATA/PFGS/", mod)
                             , full.names = FALSE
                             , recursive = FALSE)
        if (length(dirs.mod) > 0)
        {
          dirs.mod = paste0(name.simulation, "/DATA/PFGS/", mod, "/", dirs.mod)
        } else
        {
          dirs.mod = paste0(name.simulation, "/DATA/PFGS/", mod)
        }
        
        ## Get files
        files.PFG = foreach (di.mod = dirs.mod, .combine = "cbind") %do%
        {
          files.PFG = list.files(path = di.mod #paste0(name.simulation, "/DATA/PFGS/", mod)
                                 , pattern = paste0("^", mod)
                                 , full.names = TRUE)
          if (length(files.PFG) != no_PFG)
          {
            stop(paste0("There is not the same number of files "
                        , "(`.txt` file starting with `", mod, "`) "
                        , "into the DATA/PFGS/", mod, "/ folder as the number of PFG "
                        , "indicated into the file "
                        , globi))
          }
          return(data.frame(files.PFG))
        }
        assign(x = paste0("files.PFG.", mod)
               , value = files.PFG)
        assign(x = paste0("no_files.PFG.", mod)
               , value = ncol(get(paste0("files.PFG.", mod))))
      } else
      {
        assign(x = paste0("no_files.PFG.", mod), value = 0)
      }
    }
    
    no_files.PFG = sapply(MODULES, function(x) get(paste0("no_files.PFG.", x)))
    no_files.PFG = no_files.PFG[which(no_files.PFG > 0)]
    if (length(unique(no_files.PFG)) > 1)
    {
      PFG.combi = expand.grid(sapply(no_files.PFG, function(x) 1:x))
    } else
    {
      PFG.combi = as.data.frame(matrix(rep(0, length(no_files.PFG) * unique(no_files.PFG))
                                       , ncol = length(no_files.PFG)))
      colnames(PFG.combi) = names(no_files.PFG)
      for (mod in 1:length(no_files.PFG))
      {
        eval(parse(text = paste0("PFG.combi$", names(no_files.PFG)[mod], " = 1:no_files.PFG[mod]")))
      }
    }
    
    #################################################################################################
    
    if (do.DIST)
    {
      # if (!is.null(name.dist))
      # {
      if (.testParam_notChar(name.dist))
      {
        .stopMessage_beChar("name.dist") 
      } else
      {
        .testParam_existFile(paste0(name.simulation, "/DATA/MASK/", name.dist))
      }
      # } else
      # {
      # .stopMessage_beChar("name.dist") 
      # }
      nbDisturbances = .getParam(params.lines = globi
                                 , flag = "NB_DISTURBANCES"
                                 , flag.split = " "
                                 , is.num = TRUE)
    }
    
    #################################################################################################
    
    doHabSuit = .getParam(params.lines = globi
                          , flag = "DO_HAB_SUITABILITY"
                          , flag.split = " "
                          , is.num = TRUE)
    if (doHabSuit)
    {
      .testParam_existFolder(name.simulation, "DATA/PFGS/HABSUIT/")
      
      files.PFG.HS = vector()
      if (PARAMS.combi[, "PFG.HABSUIT"][i] > 0)
      {
        di = dirs.HABSUIT[PARAMS.combi$PFG.HABSUIT[i]]
        
        files.PFG.HS = list.files(path = di
                                  , full.names = TRUE)
        if (length(files.PFG.HS) != no_PFG)
        {
          warning(paste0("There is not the same number of files "
                         , "into the DATA/PFGS/HABSUIT/ folder as the number of PFG "
                         , "indicated into the file "
                         , globi))
        }
      }
    }
    
    
    #################################################################################################
    #################################################################################################    
    
    for (ii in 1:nrow(PFG.combi))
    {
      params.list = lapply(1:ncol(params.combi), function(x) { as.character(params.combi[, x]) })
      names.params.list = names.params.combi
      
      params.list = c(params.list, list(paste0(name.simulation, "/RESULTS/SIMUL_V", i, ".", ii)))
      names.params.list = c(names.params.list, "--SAVE_DIR--")
      
      params.list = c(params.list, list(files.PFG.SUCC[, PFG.combi$SUCC[ii]]))
      names.params.list = c(names.params.list, "--PFG_LIFE_HISTORY_PARAMS--")
      
      if (exists("SCENARIO.MASK"))
      {
        params.list = c(params.list, list(SCENARIO.MASK))
        names.params.list = c(names.params.list, "--MASK_CHANGE_MASK--")
      }
      
      if (do.DISP){
        params.list = c(params.list, list(files.PFG.DISP[, PFG.combi$DISP[ii]]))
        names.params.list = c(names.params.list, "--PFG_DISPERSAL_PARAMS--")
      }
      if (do.LIGHT){
        params.list = c(params.list, list(files.PFG.LIGHT[, PFG.combi$LIGHT[ii]]))
        names.params.list = c(names.params.list, "--PFG_LIGHT_PARAMS--")
      }
      if (do.SOIL){
        params.list = c(params.list, list(files.PFG.SOIL[, PFG.combi$SOIL[ii]]))
        names.params.list = c(names.params.list, "--PFG_SOIL_PARAMS--")
      }
      if (doHabSuit){
        params.list = c(params.list, list(files.PFG.HS))
        names.params.list = c(names.params.list, "--PFG_HAB_MASK--")
      }
      if (exists("SCENARIO.HS"))
      {
        params.list = c(params.list, list(SCENARIO.HS))
        names.params.list = c(names.params.list, "--HAB_CHANGE_MASK--")
      }
      if (do.DIST){
        params.list = c(params.list
                        , list(files.PFG.DIST[, PFG.combi$DIST[ii]])
                        , list(rep(paste0(name.simulation, "/DATA/MASK/", name.dist)
                                   , nbDisturbances)))
        names.params.list = c(names.params.list
                              , "--PFG_DISTURBANCES_PARAMS--"
                              , "--DIST_MASK--")
      }
      if (exists("SCENARIO.DIST"))
      {
        params.list = c(params.list, list(SCENARIO.DIST))
        names.params.list = c(names.params.list, "--DIST_CHANGE_MASK--")
      }
      
      params = c(params.list, list(""))
      names(params) = c(names.params.list, "--END_OF_FILE--")
      
      .createParams(params.file = paste0(name.simulation
                                         , "/PARAM_SIMUL/Simul_parameters_V"
                                         , i, ".", ii
                                         , ".txt")
                    , params.list = params
                    , separator = "\n")
    }
  }
}
