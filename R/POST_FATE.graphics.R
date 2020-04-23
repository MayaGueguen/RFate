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
##' abundance maps
##' @param no_years an \code{integer} corresponding to the number of simulation 
##' years that will be used to extract PFG abundance / light / soil maps
##' @param opt.ras_habitat (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the file name of a raster mask, with an 
##' \code{integer} value within each pixel, corresponding to a specific habitat
##' @param opt.doFunc.evolutionCoverage default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.evolutionPixels default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.cells_ID (\emph{optional}) default \code{NULL}. \cr The cells ID 
##' of the studied area for which PFG abundances will be extracted
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the 
##' ordinate scale will be adapted for each PFG for the graphical representation 
##' of the  evolution of abundances through time
##' @param opt.doFunc.validation default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.mat.PFG.obs a \code{data.frame} with 4 columns : \code{PFG}, 
##' \code{X}, \code{Y}, \code{obs}
##' @param opt.stratum (\emph{optional}) default \code{all}. \cr The stratum 
##' number from which to extract PFG binary maps
##' @param opt.doFunc.mapPFGvsHS default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.doFunc.mapPFG default \code{TRUE} (\emph{optional}). 
##' If \code{TRUE}, corresponding \code{POST_FATE.graphic_...} function will 
##' be run.
##' @param opt.method an \code{integer} to choose the transformation method : \cr 
##' \code{1} (relative abundance) or \code{2} (optimizing TSS) (see 
##' \code{\href{POST_FATE.binaryMaps#details}{Details}})
##' @param opt.method1.threshold default \code{0.05}. \cr If \code{method = 1}, 
##' minimum relative abundance required for each PFG to be considered as present 
##' in the concerned pixel 
##' @param opt.method2.cutoff default \code{NULL}. \cr If \code{method = 2}, a 
##' \code{data.frame} with 3 columns : \code{year}, \code{PFG}, \code{cutoff} \cr
##' (see \code{\href{POST_FATE.binaryMaps#details}{Details}})
##' @param opt.stratum_min (\emph{optional}) default \code{1}. \cr An 
##' \code{integer} corresponding to the lowest stratum from which PFG 
##' abundances will be summed up
##' @param opt.stratum_max (\emph{optional}) default \code{10}. \cr An 
##' \code{integer} corresponding to the highest stratum from which PFG 
##' abundances will be summed up
##' @param opt.doBinary (\emph{optional}) default \code{TRUE}. \cr If 
##' \code{TRUE}, abundance maps (absolute or relative) are systematically 
##' multiplied by binary maps (see 
##' \code{\href{POST_FATE.graphic_mapPFG#details}{Details}})
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
##'   each pixel (see \code{\link{POST_FATE.graphic_mapPFG}})
##'   }
##'   \item{the map of \strong{Plant Functional Group cover} for each selected 
##'   simulation year(s), representing the relative cumulated abundance of PFG 
##'   present in each pixel above a height threshold (see 
##'   \code{\link{POST_FATE.graphic_mapPFG}})
##'   }
##'   \item{the map of \strong{light Community Weighted Mean} for each selected 
##'   simulation year(s), representing the simulated value of light within each 
##'   pixel above a height threshold (see 
##'   \code{\link{POST_FATE.graphic_mapPFG}})
##'   }
##'   \item{the map of \strong{soil Community Weighted Mean} for each selected 
##'   simulation year(s), representing the simulated value of soil within each 
##'   pixel (see \code{\link{POST_FATE.graphic_mapPFG}})
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
##' \code{\link{POST_FATE.binaryMaps}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}, 
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}}, 
##' \code{\link{POST_FATE.graphic_mapPFG}}
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
  , file.simulParam # = NULL
  , years
  , no_years
  , opt.ras_habitat = NULL
  
  , opt.doFunc.evolutionCoverage = TRUE
  , opt.doFunc.evolutionPixels = TRUE
  , opt.cells_ID = NULL
  , opt.abund_fixedScale = TRUE
  
  , opt.doFunc.validation = TRUE
  , opt.mat.PFG.obs
  , opt.stratum = "all"
  
  , opt.doFunc.mapPFGvsHS = TRUE
  , opt.doFunc.mapPFG = TRUE
  , opt.method
  , opt.method1.threshold = 0.05
  , opt.method2.cutoff = NULL
  , opt.stratum_min = 1
  , opt.stratum_max = 10
  , opt.doBinary = TRUE
  
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
    if (opt.doFunc.evolutionCoverage ||
        opt.doFunc.evolutionPixels)
    {
      cat("\n ############## GET EVOLUTION PLOTS through time ############## \n")
      cat("\n >> POST_FATE.temporalEvolution...")
      cat("\n")
      POST_FATE.temporalEvolution(name.simulation = name.simulation
                                  , file.simulParam = abs.simulParam
                                  , no_years = no_years
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
    
    ## Get binary maps ------------------------------------------------------
    if (opt.doFunc.validation ||
        opt.doFunc.mapPFGvsHS ||
        opt.doFunc.mapPFG)
    {
      cat("\n ############## GET RELATIVE / BINARY MAPS and VALIDATION PLOTS ############## \n")
      cat("\n >> POST_FATE.relativeAbund...")
      cat("\n")
      POST_FATE.relativeAbund(name.simulation = name.simulation
                              , file.simulParam = abs.simulParam
                              , years = years
                              , opt.no_CPU = opt.no_CPU)
      
      if (opt.doFunc.validation)
      {
        cat("\n >> POST_FATE.graphic_validationStatistics...")
        cat("\n")
        res.validation = POST_FATE.graphic_validationStatistics(name.simulation = name.simulation
                                                                , file.simulParam = abs.simulParam
                                                                , years = years
                                                                , mat.PFG.obs = opt.mat.PFG.obs
                                                                , opt.ras_habitat = opt.ras_habitat
                                                                , opt.no_CPU = opt.no_CPU
                                                                , opt.doPlot = opt.doPlot)
      }
      if (opt.doFunc.mapPFGvsHS || opt.doFunc.mapPFG)
      {
        cat("\n >> POST_FATE.binaryMaps...")
        cat("\n")
        POST_FATE.binaryMaps(name.simulation = name.simulation
                             , file.simulParam = abs.simulParam
                             , years = years
                             , method = opt.method
                             , method1.threshold = opt.method1.threshold
                             , method2.cutoff = opt.method2.cutoff
                             , opt.no_CPU = opt.no_CPU)
      }
      if (opt.doFunc.mapPFGvsHS)
      {
        cat("\n >> POST_FATE.graphic_mapPFGvsHS...")
        cat("\n")
        res.mapPFGvsHS = POST_FATE.graphic_mapPFGvsHS(name.simulation = name.simulation
                                                      , file.simulParam = abs.simulParam
                                                      , years = years
                                                      , opt.stratum = opt.stratum
                                                      , opt.no_CPU = opt.no_CPU)
      }
      
      cat("\n ############## GET SPATIAL PLOTS for a specific year ############## \n")
      if (opt.doFunc.mapPFG)
      {
        cat("\n >> POST_FATE.graphic_mapPFG...")
        cat("\n")
        res.mapPFG = POST_FATE.graphic_mapPFG(name.simulation = name.simulation
                                              , file.simulParam = abs.simulParam
                                              , years = years
                                              , opt.stratum_min = opt.stratum_min
                                              , opt.stratum_max = opt.stratum_max
                                              , opt.doBinary = opt.doBinary
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

