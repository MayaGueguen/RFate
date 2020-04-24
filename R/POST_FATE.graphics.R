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
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' abundance maps (see \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}, 
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}})
##' @param no_years an \code{integer} corresponding to the number of simulation 
##' years that will be used to extract PFG abundance / light / soil maps (see 
##' \code{\link{POST_FATE.temporalEvolution}})
##' @param opt.ras_habitat (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the file name of a raster mask, with an 
##' \code{integer} value within each pixel, corresponding to a specific habitat 
##' (see \code{\link{POST_FATE.temporalEvolution}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}})
##' @param doFunc.evolCov default \code{TRUE}. \cr If \code{TRUE}, 
##' \code{\link{POST_FATE.graphic_evolutionCoverage}} function will be run.
##' @param doFunc.evolPix default \code{TRUE}. \cr If \code{TRUE}, 
##' \code{\link{POST_FATE.graphic_evolutionPixels}} function will be run.
##' @param evolPix.cells_ID (\emph{optional}) default \code{NULL}. \cr The cells ID 
##' of the studied area for which PFG abundances will be extracted (see 
##' \code{\link{POST_FATE.graphic_evolutionPixels}})
##' @param evol.fixedScale (\emph{optional}) default \code{TRUE}. \cr If 
##' \code{FALSE}, the ordinate scale will be adapted for each PFG for the 
##' graphical representation of the  evolution of abundances through time (see 
##' \code{\link{POST_FATE.graphic_evolutionCoverage}}, 
##' \code{\link{POST_FATE.graphic_evolutionPixels}})
##' @param doFunc.valid default \code{TRUE}. \cr If \code{TRUE}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}} function will be run.
##' @param valid.mat.PFG.obs a \code{data.frame} with 4 columns : \code{PFG}, 
##' \code{X}, \code{Y}, \code{obs} (see 
##' \code{\link{POST_FATE.graphic_validationStatistics}})
##' @param doFunc.mapPFGvsHS default \code{TRUE}. \cr If \code{TRUE}, 
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}} function will be run.
##' @param doFunc.mapPFG default \code{TRUE}. \cr If \code{TRUE}, 
##' \code{\link{POST_FATE.graphic_mapPFG}} function will be run.
##' @param mapPFGvsHS.stratum (\emph{optional}) default \code{all}. \cr The stratum 
##' number from which to extract PFG binary maps (see 
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}})
##' @param binMap.method an \code{integer} to choose the transformation method : \cr 
##' \code{1} (relative abundance) or \code{2} (optimizing TSS) (see 
##' \code{\link{POST_FATE.binaryMaps}})
##' @param binMap.method1.threshold default \code{0.05}. \cr If \code{method = 1}, 
##' minimum relative abundance required for each PFG to be considered as present 
##' in the concerned pixel (see \code{\link{POST_FATE.binaryMaps}})
##' @param binMap.method2.cutoff default \code{NULL}. \cr If \code{method = 2}, a 
##' \code{data.frame} with 3 columns : \code{year}, \code{PFG}, \code{cutoff} \cr
##' (see \code{\link{POST_FATE.binaryMaps}})
##' @param mapPFG.stratum_min (\emph{optional}) default \code{1}. \cr An 
##' \code{integer} corresponding to the lowest stratum from which PFG 
##' abundances will be summed up (see \code{\link{POST_FATE.graphic_mapPFG}})
##' @param mapPFG.stratum_max (\emph{optional}) default \code{10}. \cr An 
##' \code{integer} corresponding to the highest stratum from which PFG 
##' abundances will be summed up (see \code{\link{POST_FATE.graphic_mapPFG}})
##' @param mapPFG.doBinary (\emph{optional}) default \code{TRUE}. \cr If 
##' \code{TRUE}, abundance maps (absolute or relative) are systematically 
##' multiplied by binary maps (see \code{\link{POST_FATE.graphic_mapPFG}})
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files, as well as the extraction of values from raster files
##' 
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and a 
##' specific parameter file within this simulation, \strong{up to ten 
##' preanalytical graphics}. \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folders (\code{ABUND_perPFG_perStrata}, 
##' \code{ABUND_perPFG_allStrata}, \code{ABUND_REL_perPFG_allStrata}, 
##' \code{BIN_perPFG_perStrata}, \code{BIN_perPFG_allStrata}, \code{LIGHT} or 
##' \code{SOIL}) and unzipped. Informations extracted lead to the production of 
##' the following graphics before the maps are compressed again :
##' 
##' \itemize{
##'   \item{the evolution of \strong{space occupancy} of each plant functional 
##'   group through simulation time, \cr with \emph{space occupancy} 
##'   representing the percentage of pixels within the mask of studied area 
##'   where the PFG is present \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionCoverage}})
##'   }
##'   \item{the evolution of \strong{total abundance} of each plant functional 
##'   group through simulation time, \cr with \emph{total abundance} being the 
##'   sum over the whole studied area of the PFG abundances (\code{FATE} 
##'   \emph{arbitrary unit}) \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionCoverage}})
##'   }
##'   \item{the evolution of \strong{abundance} of each Plant Functional Group 
##'   through simulation time, within 5 (or more) randomly selected pixels of 
##'   the studied area (\code{FATE} \emph{arbitrary unit}), as well as 
##'   \strong{light resources} within each height stratum (\emph{\code{1}: Low, 
##'   \code{2}: Medium, \code{3}: High}) and \strong{soil resources} 
##'   (user-defined scale) if these modules were selected (see 
##'   \code{\link{POST_FATE.graphic_evolutionPixels}})
##'   }
##'   \item{the value of \strong{several statistics to evaluate the predictive 
##'   quality of the model} for each plant functional group \cr 
##'   (\code{\link[PresenceAbsence]{sensitivity}}, 
##'   \code{\link[PresenceAbsence]{specificity}}, 
##'   \code{\link[PresenceAbsence]{auc}}, 
##'   \code{TSS = sensitivity + specificity - 1}) (see 
##'   \code{\link{POST_FATE.graphic_validationStatistics}})
##'   }
##'   \item{the comparison between each \strong{PFG habitat suitability map and 
##'   its simulated map of presence} \cr (see 
##'   \code{\link{POST_FATE.graphic_mapPFGvsHS}})
##'   }
##'   \item{the map of \strong{PFG richness} within each pixel, representing the 
##'   sum of binary maps (see \code{\link{POST_FATE.graphic_mapPFG}})
##'   }
##'   \item{the map of \strong{PFG relative cover}, representing the sum of 
##'   relative abundance maps of all PFG \cr (potentially above a height threshold 
##'   defined by \code{opt.stratum_min}) (see 
##'   \code{\link{POST_FATE.graphic_mapPFG}})
##'   }
##'   \item{the map of \strong{light Community Weighted Mean} \cr (potentially above 
##'   a height threshold defined by \code{opt.stratum_min}) (see 
##'   \code{\link{POST_FATE.graphic_mapPFG}})
##'   }
##'   \item{the map of \strong{soil Community Weighted Mean} \cr (potentially above 
##'   a height threshold defined by \code{opt.stratum_min}) (see 
##'   \code{\link{POST_FATE.graphic_mapPFG}})
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
##'   \code{\link{POST_FATE.binaryMaps}})}
##'   \item{\file{BIN_perPFG \cr_perStrata}}{containing presence / absence 
##'   raster maps for each PFG for each stratum (see 
##'   \code{\link{POST_FATE.binaryMaps}})}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time
##' 
##' @seealso \code{\link{POST_FATE.temporalEvolution}}, 
##' \code{\link{POST_FATE.graphic_evolutionCoverage}}, 
##' \code{\link{POST_FATE.graphic_evolutionPixels}},
##' \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.binaryMaps}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}, 
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}}, 
##' \code{\link{POST_FATE.graphic_mapPFG}}
##' 
##' 
##' @examples
##' 
##' ## Load exemple data                                 
##' 
##' @export
##'
## END OF HEADER ###############################################################


POST_FATE.graphics = function(
  name.simulation
  , file.simulParam = NULL
  , years
  , no_years
  , opt.ras_habitat = NULL
  
  , doFunc.evolCov = TRUE
  , doFunc.evolPix = TRUE
  , evolPix.cells_ID = NULL
  , evol.fixedScale = TRUE
  
  , doFunc.valid = TRUE
  , valid.mat.PFG.obs
  
  , doFunc.mapPFGvsHS = TRUE
  , doFunc.mapPFG = TRUE
  , mapPFGvsHS.stratum = "all"
  , binMap.method
  , binMap.method1.threshold = 0.05
  , binMap.method2.cutoff = NULL
  , mapPFG.stratum_min = 1
  , mapPFG.stratum_max = 10
  , mapPFG.doBinary = TRUE
  
  , opt.doPlot = TRUE
  , opt.no_CPU = 1
){
  
  #############################################################################
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  
  
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get global param file directories ------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)        
    
    ## Get temporal evolution -----------------------------------------------
    if (doFunc.evolCov ||
        doFunc.evolPix)
    {
      cat("\n ############## GET EVOLUTION PLOTS through time ############## \n")
      cat("\n >> POST_FATE.temporalEvolution...")
      cat("\n")
      POST_FATE.temporalEvolution(name.simulation = name.simulation
                                  , file.simulParam = abs.simulParam
                                  , no_years = no_years
                                  , opt.ras_habitat = opt.ras_habitat
                                  , opt.no_CPU = opt.no_CPU)
      
      if (doFunc.evolCov)
      {
        cat("\n >> POST_FATE.graphic_evolutionCoverage...")
        cat("\n")
        res.evolutionCoverage = POST_FATE.graphic_evolutionCoverage(name.simulation = name.simulation
                                                                    , file.simulParam = abs.simulParam
                                                                    , evol.fixedScale = evol.fixedScale
                                                                    , opt.doPlot = opt.doPlot)
      }
      if (doFunc.evolPix)
      {
        cat("\n >> POST_FATE.graphic_evolutionPixels...")
        cat("\n")
        res.evolutionPixels = POST_FATE.graphic_evolutionPixels(name.simulation = name.simulation
                                                                , file.simulParam = abs.simulParam
                                                                , evolPix.cells_ID = evolPix.cells_ID
                                                                , evol.fixedScale = evol.fixedScale
                                                                , opt.doPlot = opt.doPlot)
      }
    }
    
    ## Get binary maps ------------------------------------------------------
    if (doFunc.valid ||
        doFunc.mapPFGvsHS ||
        doFunc.mapPFG)
    {
      cat("\n ############## GET RELATIVE / BINARY MAPS and VALIDATION PLOTS ############## \n")
      cat("\n >> POST_FATE.relativeAbund...")
      cat("\n")
      POST_FATE.relativeAbund(name.simulation = name.simulation
                              , file.simulParam = abs.simulParam
                              , years = years
                              , opt.no_CPU = opt.no_CPU)
      
      if (doFunc.valid)
      {
        cat("\n >> POST_FATE.graphic_validationStatistics...")
        cat("\n")
        res.validation = POST_FATE.graphic_validationStatistics(name.simulation = name.simulation
                                                                , file.simulParam = abs.simulParam
                                                                , years = years
                                                                , mat.PFG.obs = valid.mat.PFG.obs
                                                                , opt.ras_habitat = opt.ras_habitat
                                                                , opt.no_CPU = opt.no_CPU
                                                                , opt.doPlot = opt.doPlot)
      }
      if (doFunc.mapPFGvsHS || doFunc.mapPFG)
      {
        cat("\n >> POST_FATE.binaryMaps...")
        cat("\n")
        POST_FATE.binaryMaps(name.simulation = name.simulation
                             , file.simulParam = abs.simulParam
                             , years = years
                             , method = binMap.method
                             , method1.threshold = binMap.method1.threshold
                             , method2.cutoff = binMap.method2.cutoff
                             , opt.no_CPU = opt.no_CPU)
      }
      if (doFunc.mapPFGvsHS)
      {
        cat("\n >> POST_FATE.graphic_mapPFGvsHS...")
        cat("\n")
        res.mapPFGvsHS = POST_FATE.graphic_mapPFGvsHS(name.simulation = name.simulation
                                                      , file.simulParam = abs.simulParam
                                                      , years = years
                                                      , opt.stratum = mapPFGvsHS.stratum
                                                      , opt.no_CPU = opt.no_CPU)
      }
      
      cat("\n ############## GET SPATIAL PLOTS for a specific year ############## \n")
      if (doFunc.mapPFG)
      {
        cat("\n >> POST_FATE.graphic_mapPFG...")
        cat("\n")
        res.mapPFG = POST_FATE.graphic_mapPFG(name.simulation = name.simulation
                                              , file.simulParam = abs.simulParam
                                              , years = years
                                              , mapPFG.stratum_min = mapPFG.stratum_min
                                              , mapPFG.stratum_max = mapPFG.stratum_max
                                              , mapPFG.doBinary = mapPFG.doBinary
                                              , opt.no_CPU = opt.no_CPU
                                              , opt.doPlot = opt.doPlot)
      }
    }
    
    #########################################################################
    ## Get ALL PLOTS --------------------------------------------------------
    
    names.res = c("res.evolutionCoverage"
                  , "res.evolutionPixels"
                  , "res.validation"
                  , "res.mapPFG")
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

