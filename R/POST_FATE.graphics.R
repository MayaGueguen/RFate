### HEADER #####################################################################
##' @title Create all possible graphical representations for a \code{FATE} 
##' simulation
##' 
##' @name POST_FATE.graphics
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce a set of graphical 
##' representations for a \code{FATE} simulation. Graphics can be of three 
##' types : 1) representing an evolution through time (of abundance, light, 
##' soil) ; 2) vizualising the goodness of the modelisation (presence/absence, 
##' validation statistics) : 3) or representing a spatial distribution for a 
##' specific year (richness, abundance, light, soil).
##'              
##' @param name.simulation a \code{string} that corresponds to the main 
##' directory or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG abundance and/or binary maps, as well as light 
##' or soil resources
##' @param no.years an \code{integer} corresponding to the number of simulation 
##' years that will be used to extract PFG abundance maps
##' @param opt.strata_min default \code{1} (\emph{optional}). An \code{integer} 
##' corresponding to the lowest stratum from which PFG abundances are summed up 
##' to the highest stratum
##' @param opt.strata default \code{ALL} (\emph{optional}). The stratum number 
##' from which to extract PFG binary maps
##' @param opt.mat.PFG.succ default \code{NULL} (\emph{optional}). A 
##' \code{data.frame} with 2 or 3 columns : \code{PFG}, \code{light}, 
##' \code{soil_contrib}
##' @param opt.mat.PFG.obs default \code{NULL} (\emph{optional}). A 
##' \code{data.frame} with 4 columns : \code{PFG}, \code{X}, \code{Y}, 
##' \code{obs} (0/1)
##' @param opt.mat.CWM.obs default \code{NULL} (\emph{optional}). A 
##' \code{data.frame} with 3 or 4 columns : \code{X}, \code{Y}, \code{light} 
##' (CWM), \code{soil} (CWM)
##' @param opt.mat.cover.obs default \code{NULL} (\emph{optional}). A 
##' \code{data.frame} with 3 columns : \code{X}, \code{Y}, \code{obs} (0/1)
##' @param opt.mat.light.obs default \code{NULL} (\emph{optional}). A 
##' \code{data.frame} with 3 columns : \code{X}, \code{Y}, \code{obs} (0/1)
##' @param opt.mat.soil.obs default \code{NULL} (\emph{optional}). A 
##' \code{data.frame} with 3 columns : \code{X}, \code{Y}, \code{obs} (0/1)
##' @param opt.ras.cover.obs default \code{NULL} (\emph{optional}). A 
##' \code{string} that corresponds to the file name of a raster containing 
##' observed values for vegetation cover
##' @param opt.ras.light.obs default \code{NULL} (\emph{optional}). A 
##' \code{string} that corresponds to the file name of a raster containing 
##' observed values for community weighted mean of light
##' @param opt.ras.soil.obs default \code{NULL} (\emph{optional}). A 
##' \code{string} that corresponds to the file name of a raster containing 
##' observed values for community weighted mean of soil
##' @param opt.ras_habitat default \code{NULL} (\emph{optional}). A 
##' \code{string} that  corresponds to the file name of a raster mask, with an 
##' \code{integer} value within each pixel, corresponding to a specific habitat
##' @param opt.cells_ID default \code{NULL} (\emph{optional}). The cells ID of 
##' the studied area for which abund / light / soil resources will be extracted
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the 
##' ordinate scale will be adapted for each PFG for the graphical 
##' representation of the  evolution of abundances through time
##' @param opt.no_CPU default \code{1} (\emph{optional}). The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files
##' @param opt.doPlot default \code{TRUE} (\emph{optional}). If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and 
##' reorganization of outputs will occur, be saved and returned
##' @param opt.doFunc.evolutionCoverage default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.evolutionPixels default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.validation default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.mapPFGvsHS default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.mapPFGrichness default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.mapPFGcover default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.mapPFGlight default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.mapPFGsoil default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE} 
##' simulation and a specific parameter file within this simulation, up to ten 
##' preanalytical graphics. \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folders (\code{ABUND_perPFG_allStrata} or 
##' \code{ABUND_perPFG_allStrata} or \code{ABUND_REL_perPFG_allStrata} or 
##' \code{BIN_perPFG_perStrata} or \code{BIN_perPFG_allStrata} or \code{LIGHT} 
##' or \code{SOIL}) and unzipped.
##' Informations extracted lead to the production of the following graphics 
##' before the maps are compressed again :
##' 
##' \itemize{
##'   \item{the evolution of \strong{space occupancy} of each Plant Functional 
##'   Group through simulation time, with \emph{space occupancy} representing 
##'   the percentage of pixels within the mask of studied area where the PFG 
##'   is present (see \code{\link{POST_FATE.graphic_evolutionCoverage}})
##'   }
##'   \item{the evolution of \strong{abundance} of each Plant Functional Group 
##'   through simulation time, with \emph{abundance} being the sum over the 
##'   whole studied area of the PFG abundances (\code{FATE} \emph{arbitrary 
##'   unit}) (see \code{\link{POST_FATE.graphic_evolutionCoverage}})
##'   }
##'   \item{the evolution of \strong{abundance} of each Plant Functional Group 
##'   through simulation time, within 5 (or more) randomly selected pixels of 
##'   the studied area (\code{FATE} \emph{arbitrary unit}), as well as 
##'   \strong{light resources} of each stratum (\emph{1: Low, 2: Medium, 3: 
##'   High}) and \strong{soil resources} if these modules are selected (see 
##'   \code{\link{POST_FATE.graphic_evolutionPixels}})
##'   }
##'   \item{the value of \strong{several statistics for the predictive quality 
##'   of the model for each Plant Functional Group} and for each selected 
##'   simulation year(s) (see 
##'   \code{\link{POST_FATE.graphic_validationStatistics}})
##'   }
##'   \item{the maps of \strong{Plant Functional Group habitat suitability and 
##'   occurrences} for each selected simulation year(s), representing the 
##'   probability of presence of each PFG in each pixel compared to its 
##'   simulated presence (see \code{\link{POST_FATE.graphic_mapPFGvsHS}})
##'   }
##'   \item{the map of \strong{Plant Functional Group richness} for each 
##'   selected simulation year(s), representing the number of PFG present in 
##'   each pixel (see \code{\link{POST_FATE.graphic_mapPFGrichness}})
##'   }
##'   \item{the map of \strong{Plant Functional Group cover} for each selected 
##'   simulation year(s), representing the relative cumulated abundance of PFG 
##'   present in each pixel above a height threshold (see 
##'   \code{\link{POST_FATE.graphic_mapPFGcover}})
##'   }
##'   \item{the map of \strong{light Community Weighted Mean} for each selected 
##'   simulation year(s), representing the simulated value of light within each 
##'   pixel above a height threshold (see 
##'   \code{\link{POST_FATE.graphic_mapPFGlight}})
##'   }
##'   \item{the map of \strong{soil Community Weighted Mean} for each selected 
##'   simulation year(s), representing the simulated value of soil within each 
##'   pixel (see \code{\link{POST_FATE.graphic_mapPFGsoil}})
##'   }
##' }
##' 
##' 
##' 
##' @return The following \code{POST_FATE_[...].pdf} files are created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr spaceOccupancy}}{to visualize for each PFG the 
##'   evolution of its occupation of the studied area through simulation time}
##'   \item{\file{GRAPHIC_A \cr abundance}}{to visualize for each PFG the 
##'   evolution of its abundance within the whole studied area through 
##'   simulation time}
##'   \item{\file{GRAPHIC_A \cr pixels}}{to visualize for each PFG the 
##'   evolution of its abundance within each selected pixel through 
##'   simulation time, as well as the evolution of light and soil resources}
##'   \item{\file{GRAPHIC_B \cr validationStatistics}}{to assess the modeling 
##'   quality of each PFG based on given observations within the studied area}
##'   \item{\file{GRAPHIC_B \cr PFGvsHS}}{to visualize the PFG presence 
##'   within the studied area (probability and simulated occurrence)}
##'   \item{\file{GRAPHIC_C \cr PFGrichness}}{to visualize the PFG richness 
##'   within the studied area}
##'   \item{\file{GRAPHIC_C \cr PFGcover}}{to visualize the PFG cover 
##'   within the studied area}
##'   \item{\file{GRAPHIC_C \cr PFGlight}}{to visualize the light CWM 
##'   within the studied area}
##'   \item{\file{GRAPHIC_C \cr PFGlsoil}}{to visualize the soil CWM 
##'   within the studied area}
##' }
##' 
##' Three folders are created :
##' \describe{
##'   \item{\file{ABUND_REL_perPFG \cr_allStrata}}{containing relative 
##'   abundance raster maps for each PFG across all strata (see 
##'   \code{\link{POST_FATE.relativeAbund}})}
##'   \item{\file{BIN_perPFG \cr_allStrata}}{containing presence / absence 
##'   raster maps for each PFG across all strata (see 
##'   \code{\link{POST_FATE.graphic_validationStatistics}})}
##'   \item{\file{BIN_perPFG \cr_perStrata}}{containing presence / absence 
##'   raster maps for each PFG for each stratum (see 
##'   \code{\link{POST_FATE.graphic_validationStatistics}})}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time
##' 
##' @seealso \code{\link{POST_FATE.temporalEvolution}}, 
##' \code{\link{POST_FATE.graphic_evolutionCoverage}}, 
##' \code{\link{POST_FATE.graphic_evolutionPixels}},
##' \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}, 
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}}, 
##' \code{\link{POST_FATE.graphic_mapPFGrichness}}, 
##' \code{\link{POST_FATE.graphic_mapPFGcover}}, 
##' \code{\link{POST_FATE.graphic_mapPFGlight}}, 
##' \code{\link{POST_FATE.graphic_mapPFGsoil}}
##' 
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphics(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , year = 100
##'                                     , no.years = 10)
##'                                     
##' POST_FATE.graphics(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , year = 100
##'                                     , no.years = 10
##'                                     , opt.abund_fixedScale = FALSE
##'                                     , opt.no_CPU = 4)
##' }
##'                                     
##'                                     
##' 
##' @export
##'
## END OF HEADER ###############################################################


POST_FATE.graphics = function(
  name.simulation
  , file.simulParam
  , year
  , no.years
  , opt.strata_min = 1
  , opt.strata = "ALL"
  , opt.mat.PFG.succ = NULL
  , opt.mat.PFG.obs = NULL
  , opt.mat.CWM.obs = NULL
  , opt.mat.cover.obs = NULL
  , opt.mat.light.obs = NULL
  , opt.mat.soil.obs = NULL
  , opt.ras.cover.obs = NULL
  , opt.ras.light.obs = NULL
  , opt.ras.soil.obs = NULL
  , opt.ras_habitat = NULL
  , opt.cells_ID = NULL
  , opt.abund_fixedScale = TRUE
  , opt.no_CPU = 1
  , opt.doPlot = TRUE
  , opt.doFunc.evolutionCoverage = TRUE
  , opt.doFunc.evolutionPixels = TRUE
  , opt.doFunc.validation = TRUE
  , opt.doFunc.mapPFGvsHS = TRUE
  , opt.doFunc.mapPFGrichness = TRUE
  , opt.doFunc.mapPFGcover = TRUE
  , opt.doFunc.mapPFGlight = TRUE
  , opt.doFunc.mapPFGsoil = TRUE
){
  
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  
  if (.testParam_notDef(file.simulParam) || nchar(file.simulParam) == 0)
  {
    abs.simulParams = list.files(paste0(name.simulation, "/PARAM_SIMUL/"))
    if (length(abs.simulParams) == 0)
    {
      stop(paste0("Missing data!\n The folder ", name.simulation, "/PARAM_SIMUL/ does not contain adequate files"))
    }
    abs.simulParams = paste0(name.simulation, "/PARAM_SIMUL/", abs.simulParams)
  } else
  {
    file.simulParam = basename(file.simulParam)
    abs.simulParams = paste0(name.simulation, "/PARAM_SIMUL/", file.simulParam)
    .testParam_existFile(abs.simulParams)
  }
  if (.testParam_notNum(year))
  {
    .stopMessage_beInteger("year")
  }
  if (.testParam_notNum(no.years))
  {
    .stopMessage_beInteger("no.years")
  }
  
  #################################################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
    {
      
      cat("\n ############## GRAPHIC POST FATE ############## \n")
      cat("\n Simulation name : ", name.simulation)
      cat("\n Simulation file : ", abs.simulParam)
      cat("\n")
      
      ## Get global param file directories -----------------------------------------------------
      file.globalParam = .getParam(params.lines = abs.simulParam
                                   , flag = "GLOBAL_PARAMS"
                                   , flag.split = "^--.*--$"
                                   , is.num = FALSE)
      
      doLight <- .getParam(params.lines = paste0(sub(basename(name.simulation), "", name.simulation)
                                                 , file.globalParam)
                           , flag = "DO_LIGHT_COMPETITION"
                           , flag.split = " "
                           , is.num = TRUE)
      doSoil <- .getParam(params.lines = paste0(sub(basename(name.simulation), "", name.simulation)
                                                , file.globalParam)
                          , flag = "DO_SOIL_COMPETITION"
                          , flag.split = " "
                          , is.num = TRUE)
      
      ## Get temporal evolution -----------------------------------------------------------------------
      if (opt.doFunc.evolutionCoverage ||
          opt.doFunc.evolutionPixels)
      {
        cat("\n ############## GET EVOLUTION PLOTS through time ############## \n")
        cat("\n >> POST_FATE.temporalEvolution...")
        cat("\n")
        POST_FATE.temporalEvolution(name.simulation = name.simulation
                                    , file.simulParam = abs.simulParam
                                    , no.years = no.years
                                    , opt.ras_habitat = opt.ras_habitat
                                    , opt.no_CPU = opt.no_CPU)
        
        if (opt.doFunc.evolutionCoverage)
        {
          cat("\n >> POST_FATE.graphic_evolutionCoverage...")
          cat("\n")
          res.evolutionCoverage = POST_FATE.graphic_evolutionCoverage(name.simulation = name.simulation
                                                                      , file.simulParam = abs.simulParam
                                                                      , opt.abund_fixedScale = opt.abund_fixedScale
                                                                      , opt.doPlot = opt.doPlot)
        }
        if (opt.doFunc.evolutionPixels)
        {
          cat("\n >> POST_FATE.graphic_evolutionPixels...")
          cat("\n")
          res.evolutionPixels = POST_FATE.graphic_evolutionPixels(name.simulation = name.simulation
                                                                  , file.simulParam = abs.simulParam
                                                                  , opt.cells_ID = opt.cells_ID
                                                                  , opt.abund_fixedScale = opt.abund_fixedScale
                                                                  , opt.doPlot = opt.doPlot)
        }
      }
      
      ## Get binary maps -----------------------------------------------------------------------
      if (opt.doFunc.validation ||
          opt.doFunc.mapPFGvsHS ||
          opt.doFunc.mapPFGrichness ||
          opt.doFunc.mapPFGcover ||
          opt.doFunc.mapPFGlight ||
          opt.doFunc.mapPFGsoil)
      {
        cat("\n ############## GET RELATIVE / BINARY MAPS and VALIDATION PLOTS ############## \n")
        cat("\n >> POST_FATE.relativeAbund...")
        cat("\n")
        POST_FATE.relativeAbund(name.simulation = name.simulation
                                , file.simulParam = abs.simulParam
                                , year = year
                                , opt.no_CPU = opt.no_CPU)
        
        if (opt.doFunc.validation)
        {
          cat("\n >> POST_FATE.graphic_validationStatistics...")
          cat("\n")
          res.validation = POST_FATE.graphic_validationStatistics(name.simulation = name.simulation
                                                                  , file.simulParam = abs.simulParam
                                                                  , year = year
                                                                  , mat.PFG.obs = opt.mat.PFG.obs
                                                                  , opt.ras_habitat = opt.ras_habitat
                                                                  , opt.no_CPU = opt.no_CPU
                                                                  , opt.doPlot = opt.doPlot)
        }
        if (opt.doFunc.mapPFGvsHS)
        {
          cat("\n >> POST_FATE.graphic_mapPFGvsHS...")
          cat("\n")
          res.mapPFGvsHS = POST_FATE.graphic_mapPFGvsHS(name.simulation = name.simulation
                                                        , file.simulParam = abs.simulParam
                                                        , year = year
                                                        , opt.no_CPU = opt.no_CPU
                                                        , opt.strata = "all")
        }
        
        cat("\n ############## GET SPATIAL PLOTS for a specific year ############## \n")
        if (opt.doFunc.mapPFGrichness)
        {
          cat("\n >> POST_FATE.graphic_mapPFGrichness...")
          cat("\n")
          res.mapPFGrichness = POST_FATE.graphic_mapPFGrichness(name.simulation = name.simulation
                                                                , file.simulParam = abs.simulParam
                                                                , year = year
                                                                , opt.no_CPU = opt.no_CPU
                                                                , opt.doPlot = opt.doPlot)
        }
        if (opt.doFunc.mapPFGcover)
        {
          cat("\n >> POST_FATE.graphic_mapPFGcover...")
          cat("\n")
          res.mapPFGcover = POST_FATE.graphic_mapPFGcover(name.simulation = name.simulation
                                                          , file.simulParam = abs.simulParam
                                                          , year = year
                                                          , strata_min = opt.strata_min
                                                          , opt.mat.cover.obs = opt.mat.cover.obs
                                                          , opt.ras.cover.obs = opt.ras.cover.obs
                                                          , opt.no_CPU = opt.no_CPU
                                                          , opt.doPlot = opt.doPlot)
        }
        if (opt.doFunc.mapPFGlight && doLight)
        {
          cat("\n >> POST_FATE.graphic_mapPFGlight...")
          cat("\n")
          res.mapPFGlight = POST_FATE.graphic_mapPFGlight(name.simulation = name.simulation
                                                          , file.simulParam = abs.simulParam
                                                          , year = year
                                                          , strata_min = opt.strata_min
                                                          , mat.PFG.succ = opt.mat.PFG.succ
                                                          , opt.mat.light.obs = opt.mat.light.obs
                                                          , opt.ras.light.obs = opt.ras.light.obs
                                                          , opt.no_CPU = opt.no_CPU
                                                          , opt.doPlot = opt.doPlot)
        }
        if (opt.doFunc.mapPFGsoil && doSoil)
        {
          cat("\n >> POST_FATE.graphic_mapPFGsoil...")
          cat("\n")
          res.mapPFGsoil = POST_FATE.graphic_mapPFGsoil(name.simulation = name.simulation
                                                        , file.simulParam = abs.simulParam
                                                        , year = year
                                                        , strata_min = opt.strata_min
                                                        , mat.PFG.succ = opt.mat.PFG.succ
                                                        , opt.mat.soil.obs = opt.mat.soil.obs
                                                        , opt.ras.soil.obs = opt.ras.soil.obs
                                                        , opt.no_CPU = opt.no_CPU
                                                        , opt.doPlot = opt.doPlot)
        }
      }
      
      ## Get ALL PLOTS -----------------------------------------------------------------------
      
      names.res = c("res.validation"
                    , "res.mapPFGvsHS"
                    , "res.mapPFGcover"
                    , "res.mapPFGrichness"
                    , "res.mapPFGlight"
                    , "res.mapPFGsoil"
                    , "res.evolutionCoverage"
                    , "res.evolutionPixels")
      res = list()
      for(i in names.res)
      {
        if (exists(i))
        {
          res[[i]] = get(i)
        }
      }
      return(res)
    } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

