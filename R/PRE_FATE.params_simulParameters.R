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
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE-HD} simulation
##' @param name.MASK a \code{string} corresponding to the file name of a raster 
##' mask, with either \code{0} or \code{1} within each pixel, \code{1} 
##' corresponding to the cells of the studied area in which the succession 
##' (core) module of the \code{FATE-HD} simulation will take place (see 
##' \code{\href{PRE_FATE.params_globalParameters.html#details}{PRE_FATE.params_globalParameters}})
##' @param name.DIST (\emph{optional}) \cr a \code{string} corresponding to the 
##' file name of a raster mask, with either \code{0} or \code{1} within each 
##' pixel, \code{1} corresponding to the cells of the studied area in which the 
##' disturbance module of the \code{FATE-HD} simulation will take place (see 
##' \code{\href{PRE_FATE.params_globalParameters.html#details}{PRE_FATE.params_globalParameters}})
## @param name.HABSTAB (\emph{optional}) \cr a \code{vector} containing 2 
## \code{string} corresponding to :
## \enumerate{
##   \item the name of a raster file, with an \code{integer} within each pixel 
##   corresponding to the ID of its habitat
##   \item the name of a \code{.txt} file, containing baseline statistics for 
##   each habitat defined in the above-mentioned raster
## }
## These files will be used by the habitat stability module of the 
## \code{FATE-HD} simulation (see 
## \code{\href{PRE_FATE.params_globalParameters.html#details}{PRE_FATE.params_globalParameters}})
##' @param name.DROUGHT (\emph{optional}) \cr a \code{string} corresponding to 
##' the name of a raster file, with a \code{numeric} value within each pixel 
##' corresponding to the drought intensity experienced by this pixel throught 
##' the drought disturbance module of the \code{FATE-HD} simulation (see 
##' \code{\href{PRE_FATE.params_globalParameters.html#details}{PRE_FATE.params_globalParameters}})
##' @param opt.global.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the global parameter file in the folder 
##' \code{name.simulation/DATA/GLOBAL_PARAMETERS/} that will be used to build 
##' the simulation parameter file
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder in each \code{name.simulation/DATA/PFGS/module/} 
##' from which PFG file names will be extracted to build the simulation 
##' parameter file
##' 
##' 
##' 
##' @details 
##' 
##' The \code{FATE-HD} software takes only one input parameter : a file 
##' containing links to other files containing all the parameters and data 
##' needed by the program to run.
##' 
##' 
##' \describe{
##'   \item{GLOBAL_PARAMS}{file where parameters related to the simulation 
##'   definition are referred (e.g. number of PFG involved, number of height 
##'   strata, simulation duration, computer resources, modules loaded, etc) \cr
##'   (see \code{\link{PRE_FATE.params_globalParameters}}) \cr \cr
##'   }
##'   \item{SAVING_DIR}{directory where simulation outputs will be stored}
##'   \item{SAVING_YEARS_ \cr ARRAYS (\emph{optional})}{file containing the 
##'   years for which simulation maps will be saved \cr
##'   (see \code{\link{PRE_FATE.params_saveYears}})
##'   }
##'   \item{SAVING_YEARS_ \cr OBJECTS (\emph{optional})}{file containing the 
##'   years for which simulation outputs will be saved \cr
##'   (see \code{\link{PRE_FATE.params_saveYears}}) \cr \cr
##'   }
##'   \item{MASK}{raster mask that will define the study area}
##'   \item{MASK_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the years 
##'   to change rasters for the succession module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{MASK_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the files 
##'   to change rasters for the succession module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr LIFE_HISTORY}{PFG life history related parameters 
##'   (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGsuccession}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr LIGHT (\emph{optional})}{PFG light preferences and 
##'   tolerance related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGlight}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr SOIL (\emph{optional})}{PFG soil contribution and 
##'   tolerance related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGsoil}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr DISPERSAL (\emph{optional})}{PFG dispersal 
##'   capacity related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdispersal}}) \cr \cr
##'   }
##'   \item{PFG_MASK_HABSUIT \cr (\emph{optional})}{raster masks (one by PFG) 
##'   containing PFG habitat suitability for the study area}
##'   \item{HABSUIT_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the years 
##'   to change rasters for the habitat suitability module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{HABSUIT_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the files 
##'   to change rasters for the habitat suitability module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr DISTURBANCES (\emph{optional})}{PFG disturbance 
##'   related parameters in terms of resprouting and mortality (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdisturbance}})
##'   }
##'   \item{DIST_MASK \cr (\emph{optional})}{raster masks that will define the 
##'   disturbance areas}
##'   \item{DIST_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the years 
##'   to change rasters for the disturbance module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{DIST_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the files 
##'   to change rasters for the disturbance module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##   \item{HABSTAB_MASK \cr (\emph{optional})}{raster mask that will define the 
##   habitat areas}
##   \item{HABSTAB_BASELINE \cr (\emph{optional})}{tobefilled \cr \cr}
##'   \item{PFG_PARAMS_DROUGHT \cr (\emph{optional})}{PFG drought disturbance 
##'   related parameters in terms of resprouting and mortality (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdrought}})}
##'   \item{DROUGHT_MASK \cr (\emph{optional})}{raster mask that will define the 
##'   drought intensity of the area}
##'   \item{DROUGHT_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change rasters for the drought disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{DROUGHT_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the 
##'   files to change rasters for the drought disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr}
##'   \item{PFG_MASK_ALIENS \cr (\emph{optional})}{raster masks (one by alien) 
##'   containing alien introduction zones for the study area}
##'   \item{ALIENS_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change rasters for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ALIENS_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the 
##'   files to change rasters for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ALIENS_CHANGEFREQ_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change frequencies for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ALIENS_CHANGEFREQ_FILES \cr (\emph{optional})}{file containing the 
##'   files to change frequencies for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##' }
##' 
##' 
##' 
##' @note 
##' 
##' \itemize{
##'   \item The function produces links to files that are \strong{NOT absolute 
##'   paths BUT relative ones}. \cr
##'   \emph{When relative paths are used}, the user should be careful of the 
##'   folder from which the simulation is launched. \cr
##'   \emph{A function allows to transform these relative paths into absolute 
##'   ones. (see examples of \code{\link{.setPattern}})}.
##'   \item \strong{The order of files matters!} \cr
##'   For instance the first link below \code{--PFG_PARAMS_LIFE_HISTORY--} flag 
##'   (e.g. \emph{PFG Albert}) has to match with the first item below the 
##'   \code{--PFG_PARAMS_DISPERSAL--} flag (must be \emph{PFG Albert} too).
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
##'   \item \strong{--SAVING_DIR--}
##'   \itemize{
##'   \item --SAVING_YEARS_ARRAYS-- (\emph{optional})
##'   \item --SAVING_YEARS_OBJECTS-- (\emph{optional})
##'   }
##'   \item \strong{--MASK--}
##'   \itemize{
##'   \item --MASK_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --MASK_CHANGEMASK_FILES-- (\emph{optional})
##'   }
##'   \item \strong{--PFG_PARAMS_LIFE_HISTORY--}
##'   \itemize{
##'   \item --PFG_PARAMS_LIGHT-- (\emph{optional})
##'   \item --PFG_PARAMS_SOIL-- (\emph{optional})
##'   \item --PFG_PARAMS_DISPERSAL-- (\emph{optional})
##'   \item --PFG_MASK_HABSUIT-- (\emph{optional})
##'   \item --HABSUIT_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --HABSUIT_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --PFG_PARAMS_DISTURBANCES-- (\emph{optional})
##'   \item --DIST_MASK-- (\emph{optional})
##'   \item --DIST_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --DIST_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --PFG_PARAMS_DROUGHT-- (\emph{optional})
##'   \item --DROUGHT_MASK-- (\emph{optional})
##'   \item --DROUGHT_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --DROUGHT_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --PFG_MASK_ALIENS-- (\emph{optional})
##'   \item --ALIENS_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --ALIENS_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --ALIENS_CHANGEFREQ_YEARS-- (\emph{optional})
##'   \item --ALIENS_CHANGEFREQ_FILES-- (\emph{optional})
##' }
##'   \item \strong{--END_OF_FILE--}
##' }
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' \code{\link{PRE_FATE.params_PFGsoil}},
##' \code{\link{PRE_FATE.params_PFGdispersal}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}},
##' \code{\link{PRE_FATE.params_PFGdrought}},
##' \code{\link{PRE_FATE.params_saveYears}},
##' \code{\link{PRE_FATE.params_changingYears}},
##' \code{\link{.setPattern}}
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
##'                                                         , d50 = rep(c(50, 50, 10),2)
##'                                                         , d99 = rep(c(1000, 1500, 2000),2)
##'                                                         , ldd = rep(c(10000, 5000, 10000), 2)))
##' 
##' ## Create PFG disturbance parameter files
##' tab.dist = data.frame(name = rep(c("DIST1","DIST2"), each = 4 * 3)
##'                       , responseStage = rep(1:4, 2 * 3)
##'                       , PFG = rep(c("C", "H", "P"), each = 2 * 4)
##'                       , KilledIndiv = c(c(0,10,10,10,1,1,0,0)
##'                                         , c(0,0,0,0,1,1,0,0)
##'                                         , c(10,10,10,10,10,0,0,0))
##'                       , ResproutIndiv = c(c(0,0,0,0,0,0,5,1)
##'                                           , c(0,0,9,10,0,0,5,1)
##'                                           , c(0,0,0,0,0,0,0,0)))
##' 
##' PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
##'                                , mat.PFG.dist = tab.dist)
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
##'   file.create(paste0("FATE_simulation/DATA/PFGS/HABSUIT/mask_PFG", pfg, ".tif"))
##' }
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
##'                                 , name.mask = "mask.tif")
##'            
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
##'                                  , required.no_PFG = PNE_PARAM$global["NO_FG"]
##'                                  , required.no_STRATA = PNE_PARAM$global["NO_STRATUM"]
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
##' @export
##' 
##' @importFrom foreach foreach %do%
##'
## END OF HEADER ###############################################################


PRE_FATE.params_simulParameters = function(
  name.simulation
  , name.MASK
  , name.DIST = NULL
  , name.DROUGHT = NULL
  , opt.global.name = NULL
  , opt.folder.name = NULL
){
  
  ############################################################################# 
  
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "DATA/GLOBAL_PARAMETERS/")
  .testParam_existFolder(name.simulation, "DATA/SAVE/")
  .testParam_existFolder(name.simulation, "DATA/SCENARIO/")
  .testParam_existFolder(name.simulation, "DATA/MASK/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/SUCC/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_notChar.m("name.MASK", name.MASK)
  .testParam_existFile(paste0(name.simulation, "/DATA/MASK/", name.MASK))
  ## CHECK parameter opt.global.name
  opt.global.name = .getParam_opt.folder.name(opt.global.name
                                              , basename(opt.global.name)
                                              , create.dir = FALSE)
  opt.global.name = sub("/$", "", opt.global.name)
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(opt.folder.name, "/")
                                              , create.dir = FALSE)
  
  type.changing1 = c("MASK", "HABSUIT", "DIST", "DROUGHT", "ALIENS") #, "ALIENS_F")
  type.changing2 = c("HABSUIT", "ALIENS")
  
  
  #############################################################################
  ## Get the name(s) of global parameter file(s) that will
  ## be used to create the simulation parameter file(s)
  
  if (opt.global.name == "")
  {
    files.GLOBAL = list.files(path = paste0(name.simulation
                                            , "/DATA/GLOBAL_PARAMETERS")
                              , pattern = "^Global_parameters.*.txt"
                              , full.names = TRUE)
  } else
  {
    files.GLOBAL = paste0(name.simulation
                          , "/DATA/GLOBAL_PARAMETERS/"
                          , opt.global.name)
  }
  if (length(files.GLOBAL) == 0)
  {
    stop(paste0("Wrong number of files!\n There is no adequate file "
                , "(`.txt` file starting with `Global_parameters`) "
                , "into the DATA/GLOBAL_PARAMETERS/ folder"))
  }
  
  #############################################################################
  ## Get the name(s) of SAVE directory
  
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
  
  #############################################################################
  ## Get the name(s) of SCENARIO directory
  
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
  
  for (ty in type.changing1)
  {
    assign(x = paste0("dirs.SCENARIO.", ty), value = vector())
    for (di in dirs.SCENARIO)
    {
      files.ty = list.files(path = di
                            , pattern = paste0("^", ty, "_changingmask_years")
                            , full.names = TRUE)
      if (length(files.ty) > 0)
      {
        eval(parse(text = paste0("dirs.SCENARIO.", ty
                                 , " = c(dirs.SCENARIO.", ty
                                 , ", di)")))
      } else
      {
        warning(paste0("There is no adequate file (`.txt` file starting with `"
                       , ty, "_changingmask_years`) "
                       , "into the folder ", di))
      }
    }
  }
  
  #############################################################################
  ## Get the name(s) of HABSUIT / ALIENS directory
  
  for (ty in type.changing2)
  {
    name.dir = paste0(name.simulation, "/DATA/PFGS/", ty)
    dirs.SCE = list.dirs(path = name.dir
                         , full.names = FALSE
                         , recursive = FALSE)
    if (length(dirs.SCE) > 0)
    {
      dirs.SCE = paste0(name.dir, "/", dirs.SCE)
    } else
    {
      dirs.SCE = name.dir
    }
    
    assign(x = paste0("dirs.", ty), value = vector())
    for (di in dirs.SCE)
    {
      files.di = list.files(path = di, full.names = TRUE)
      if (length(files.di) > 0)
      {
        eval(parse(text = paste0("dirs.", ty, " = c(dirs.", ty, ", di)")))
      }
    }
  }
  
  #############################################################################
  #############################################################################
  ## Combine the names found (global parameters, SAVE directory, 
  ## SCENARIO directory, HABSUIT directory)
  
  sce.mask = sce.habsuit = sce.dist = sce.drought = sce.aliens = 0
  ras.habsuit = ras.aliens = 0
  if (length(dirs.SCENARIO.MASK) > 0) sce.mask = 1:length(dirs.SCENARIO.MASK)
  if (length(dirs.SCENARIO.HABSUIT) > 0) sce.habsuit = 1:length(dirs.SCENARIO.HABSUIT)
  if (length(dirs.SCENARIO.DIST) > 0) sce.dist = 1:length(dirs.SCENARIO.DIST)
  if (length(dirs.SCENARIO.DROUGHT) > 0) sce.drought = 1:length(dirs.SCENARIO.DROUGHT)
  if (length(dirs.SCENARIO.ALIENS) > 0) sce.aliens = 1:length(dirs.SCENARIO.ALIENS)
  if (length(dirs.HABSUIT) > 0) ras.habsuit = 1:length(dirs.HABSUIT)
  if (length(dirs.ALIENS) > 0) ras.aliens = 1:length(dirs.ALIENS)
  
  PARAMS.combi = expand.grid(GLOBAL = 1:length(files.GLOBAL)
                             , SAVE = 1:length(dirs.SAVE)
                             , SCENARIO.MASK = sce.mask
                             , SCENARIO.HABSUIT = sce.habsuit
                             , SCENARIO.DIST = sce.dist
                             , SCENARIO.DROUGHT = sce.drought
                             , SCENARIO.ALIENS = sce.aliens
                             , PFG.HABSUIT = ras.habsuit
                             , PFG.ALIENS = ras.aliens)
  
  #############################################################################
  #############################################################################
  ## Build a simulation parameter file for each combination found
  
  for (i in 1:nrow(PARAMS.combi))
  {
    
    params.combi = data.frame(GLOBAL = files.GLOBAL[PARAMS.combi$GLOBAL[i]]
                              , MASK = paste0(name.simulation
                                              , "/DATA/MASK/"
                                              , name.MASK)
                              , stringsAsFactors = FALSE)
    names.params.combi = c("--GLOBAL_PARAMS--", "--MASK--")
    
    #############################################################################
    
    di = dirs.SAVE[PARAMS.combi$SAVE[i]]
    di.opt = data.frame(pat = c("SAVE_YEARS_maps", "SAVE_YEARS_objects")
                        , nam = c("SAVE.maps", "SAVE.obj")
                        , param = c("SAVING_YEARS_ARRAYS", "SAVING_YEARS_OBJECTS")
                        , stringsAsFactors = FALSE)
    
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
        params.combi[[di.opt$nam[ii]]] = files.found
        names.params.combi = c(names.params.combi
                               , paste0("--", di.opt$param[ii], "--"))
      } else
      {
        stop(paste0("There is too many adequate files (`.txt` file starting with `"
                    , di.opt$pat[ii], "`) "
                    , "into the folder ", di))
      }
    }
    
    #############################################################################
    
    for (ty in type.changing1)
    {
      if (PARAMS.combi[, paste0("SCENARIO.", ty)][i] > 0)
      {
        eval(parse(text = paste0("di = dirs.SCENARIO.", ty
                                 , "[PARAMS.combi$SCENARIO.", ty, "[i]]")))
        
        ## Changing years
        files.SCE.years = list.files(path = di
                                     , pattern = paste0("^", ty, "_changingmask_years")
                                     , full.names = TRUE)
        if (length(files.SCE.years) == 1)
        {
          params.combi[[paste0("SCENARIO.", ty)]] = files.SCE.years
          names.params.combi = c(names.params.combi
                                 , paste0("--", ty, "_CHANGEMASK_YEARS--"))
        } else
        {
          stop(paste0("There is too many adequate files (`.txt` file starting with `"
                      , ty, "_changingmask_years`) "
                      , "into the folder ", di))
        }
        
        ## Changing files
        files.SCE.masks = list.files(path = di
                                     , pattern = paste0("^", ty, "_changingmask_files")
                                     , full.names = TRUE)
        if (length(files.SCE.masks) == 0)
        {
          warning(paste0("There is no adequate file (`.txt` file starting with `"
                         , ty, "_changingmask_files`) "
                         , "into the folder ", di))
        } else if (length(files.SCE.masks) > 0)
        {
          assign(x = paste0("SCENARIO.", ty), value = files.SCE.masks)
        }
      }
    }
    
    #############################################################################
    
    globi = as.character(files.GLOBAL[PARAMS.combi$GLOBAL[i]])
    
    ### -------------------------------------------------------------------- ###
    
    no_PFG = .getParam(params.lines = globi
                       , flag = "NO_PFG"
                       , flag.split = " "
                       , is.num = TRUE)
    no_PFG = as.numeric(no_PFG)
    
    do.SUCC = 1
    do.LIGHT = .getParam(params.lines = globi
                         , flag = "DO_LIGHT_COMPETITION"
                         , flag.split = " "
                         , is.num = TRUE)
    do.SOIL = .getParam(params.lines = globi
                        , flag = "DO_SOIL_COMPETITION"
                        , flag.split = " "
                        , is.num = TRUE)
    do.DISP = .getParam(params.lines = globi
                        , flag = "DO_DISPERSAL"
                        , flag.split = " "
                        , is.num = TRUE)
    do.HABSUIT = .getParam(params.lines = globi
                           , flag = "DO_HAB_SUITABILITY"
                           , flag.split = " "
                           , is.num = TRUE)
    do.DIST = .getParam(params.lines = globi
                        , flag = "DO_DISTURBANCES"
                        , flag.split = " "
                        , is.num = TRUE)
    do.DROUGHT = .getParam(params.lines = globi
                           , flag = "DO_DROUGHT_DISTURBANCE"
                           , flag.split = " "
                           , is.num = TRUE)
    do.ALIENS = .getParam(params.lines = globi
                          , flag = "DO_ALIENS_INTRODUCTION"
                          , flag.split = " "
                          , is.num = TRUE)
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("SUCC", "LIGHT", "SOIL", "DISP", "DIST", "DROUGHT")
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_existFolder(name.simulation, paste0("DATA/PFGS/", mod, "/"))
        name.dir = paste0(name.simulation, "/DATA/PFGS/", mod)
        
        ## Get folders
        if (opt.folder.name != "")
        {
          dirs.mod = paste0(name.dir, "/", opt.folder.name)
        }
        if (opt.folder.name == "" || !dir.exists(dirs.mod))
        {
          dirs.mod = list.dirs(path = name.dir
                               , full.names = FALSE
                               , recursive = FALSE)
          if (length(dirs.mod) > 0)
          {
            dirs.mod = paste0(name.dir, "/", dirs.mod)
          } else
          {
            dirs.mod = name.dir
          }
        }
        
        ## Get files
        files.PFG = foreach (di.mod = dirs.mod, .combine = "cbind") %do%
        {
          files.PFG = list.files(path = di.mod
                                 , pattern = paste0("^", mod)
                                 , full.names = TRUE)
          if (length(files.PFG) != no_PFG)
          {
            stop(paste0("There is not the same number of files "
                        , "(`.txt` file starting with `", mod, "`) "
                        , "into the ", di.mod, "/ folder as the number of PFG "
                        , "indicated into the file "
                        , globi))
          }
          return(data.frame(files.PFG, stringsAsFactors = FALSE))
        }
        assign(x = paste0("files.PFG.", mod), value = files.PFG)
        assign(x = paste0("no_files.PFG.", mod)
               , value = ncol(get(paste0("files.PFG.", mod))))
      } else
      {
        assign(x = paste0("files.PFG.", mod), value = "")
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
      PFG.combi = matrix(rep(0, length(no_files.PFG) * unique(no_files.PFG))
                         , ncol = length(no_files.PFG))
      PFG.combi = as.data.frame(PFG.combi)
      colnames(PFG.combi) = names(no_files.PFG)
      for (mod in 1:length(no_files.PFG))
      {
        PFG.combi[[names(no_files.PFG)[mod]]] = 1:no_files.PFG[mod]
      }
    }
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("DIST", "DROUGHT")
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_notChar.m(paste0("name.", mod), get(paste0("name.", mod)))
        .testParam_existFile(paste0(name.simulation
                                    , "/DATA/MASK/"
                                    , get(paste0("name.", mod))))
        assign(x = paste0("no.", mod)
               , value = .getParam(params.lines = globi
                                   , flag = paste0(mod, "_NO")
                                   , flag.split = " "
                                   , is.num = TRUE))
      } else
      {
        assign(x = paste0("no.", mod), value = 0)
      }
    }
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("HABSUIT", "ALIENS")
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_existFolder(name.simulation, paste0("DATA/PFGS/", mod, "/"))
        
        if (PARAMS.combi[[paste0("PFG.", mod)]][i] > 0)
        {
          eval(parse(text = paste0("di = dirs.", mod
                                   , "[PARAMS.combi$PFG.", mod, "[i]]")))
          assign(x = paste0("files.PFG.", mod)
                 , value = list.files(path = di, full.names = TRUE))
          if (length(get(paste0("files.PFG.", mod))) != no_PFG)
          {
            warning(paste0("There is not the same number of files into the "
                           , "DATA/PFGS/", mod, "/ folder as the number of PFG "
                           , "indicated into the file "
                           , globi))
          }
        } else
        {
          assign(x = paste0("files.PFG.", mod), value = "")
        }
      } else
      {
        assign(x = paste0("files.PFG.", mod), value = "")
      }
    }
    
    #############################################################################
    
    FILE_NUMBER = 0
    if (nrow(PARAMS.combi) == 1 && nrow(PFG.combi) == 1)
    {
      FILE_NUMBER = length(list.files(path = paste0(name.simulation, "/PARAM_SIMUL/")
                                      , pattern = "^Simul_parameters_V"
                                      , recursive = FALSE
                                      , include.dirs = FALSE)) + 1
    }
    
    #############################################################################
    #############################################################################
    
    for (ii in 1:nrow(PFG.combi))
    {
      params.list = lapply(1:ncol(params.combi)
                           , function(x) { as.character(params.combi[, x]) })
      names.params.list = names.params.combi
      
      if (FILE_NUMBER == 0)
      {
        params.list = c(params.list, list(paste0(name.simulation
                                                 , "/RESULTS/SIMUL_V"
                                                 , i, ".", ii)))
      } else
      {
        params.list = c(params.list, list(paste0(name.simulation
                                                 , "/RESULTS/SIMUL_V"
                                                 , FILE_NUMBER)))
      }
      names.params.list = c(names.params.list, "--SAVING_DIR--")
      
      params.list = c(params.list, list(files.PFG.SUCC[, PFG.combi$SUCC[ii]]))
      names.params.list = c(names.params.list, "--PFG_PARAMS_LIFE_HISTORY--")
      
      if (exists("SCENARIO.MASK"))
      {
        params.list = c(params.list, list(SCENARIO.MASK))
        names.params.list = c(names.params.list, "--MASK_CHANGEMASK_FILES--")
      }
      
      if (do.LIGHT)
      {
        params.list = c(params.list, list(files.PFG.LIGHT[, PFG.combi$LIGHT[ii]]))
        names.params.list = c(names.params.list, "--PFG_PARAMS_LIGHT--")
      }
      if (do.SOIL)
      {
        params.list = c(params.list, list(files.PFG.SOIL[, PFG.combi$SOIL[ii]]))
        names.params.list = c(names.params.list, "--PFG_PARAMS_SOIL--")
      }
      if (do.DISP)
      {
        params.list = c(params.list, list(files.PFG.DISP[, PFG.combi$DISP[ii]]))
        names.params.list = c(names.params.list, "--PFG_PARAMS_DISPERSAL--")
      }
      if (do.HABSUIT)
      {
        params.list = c(params.list, list(files.PFG.HABSUIT))
        names.params.list = c(names.params.list, "--PFG_MASK_HABSUIT--")
        if (exists("SCENARIO.HABSUIT"))
        {
          params.list = c(params.list, list(SCENARIO.HABSUIT))
          names.params.list = c(names.params.list, "--HABSUIT_CHANGEMASK_FILES--")
        }
      }
      if (do.DIST)
      {
        params.list = c(params.list
                        , list(files.PFG.DIST[, PFG.combi$DIST[ii]])
                        , list(rep(paste0(name.simulation
                                          , "/DATA/MASK/"
                                          , name.DIST)
                                   , no.DIST)))
        names.params.list = c(names.params.list
                              , "--PFG_PARAMS_DISTURBANCES--"
                              , "--DIST_MASK--")
        if (exists("SCENARIO.DIST"))
        {
          params.list = c(params.list, list(SCENARIO.DIST))
          names.params.list = c(names.params.list, "--DIST_CHANGEMASK_FILES--")
        }
      }
      if (do.DROUGHT)
      {
        params.list = c(params.list
                        , list(files.PFG.DROUGHT[, PFG.combi$DROUGHT[ii]])
                        , list(paste0(name.simulation
                                      , "/DATA/MASK/"
                                      , name.DROUGHT)))
        names.params.list = c(names.params.list
                              , "--PFG_PARAMS_DROUGHT--"
                              , "--DROUGHT_MASK--")
        if (exists("SCENARIO.DROUGHT"))
        {
          params.list = c(params.list, list(SCENARIO.DROUGHT))
          names.params.list = c(names.params.list, "--DROUGHT_CHANGEMASK_FILES--")
        }
      }
      if (do.ALIENS)
      {
        params.list = c(params.list, list(files.PFG.ALIENS))
        names.params.list = c(names.params.list, "--PFG_MASK_ALIENS--")
        if (exists("SCENARIO.ALIENS"))
        {
          params.list = c(params.list, list(SCENARIO.ALIENS))
          names.params.list = c(names.params.list, "--ALIENS_CHANGEMASK_FILES--")
        }
      }
      
      ### -------------------------------------------------------------------- ###
      
      params = c(params.list, list(""))
      names(params) = c(names.params.list, "--END_OF_FILE--")
      
      .createParams(params.file = paste0(name.simulation
                                         , "/PARAM_SIMUL/Simul_parameters_V"
                                         , ifelse(FILE_NUMBER == 0
                                                  , paste0(i, ".", ii)
                                                  , FILE_NUMBER)
                                         , ".txt")
                    , params.list = params
                    , separator = "\n")
      
    }
  }
}
