### HEADER #####################################################################
##' @title Create a map of the Plant Functional Group richness 
##'  \cr for one (or several) specific year of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_mapPFGrichness
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce a raster map of PFG richness
##' for one (or several) specific \code{FATE-HD} simulation year.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG binary maps
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' @param opt.doPlot default TRUE (\emph{optional}). If TRUE, plot(s) will be
##' processed, otherwise only the calculation and reorganization of outputs
##' will occur, be saved and returned.
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, one preanalytical
##' graphic. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{BIN_perPFG_allStrata} and unzipped.
##' Informations extracted lead to the production of one graphic before the
##' maps are compressed again :
##' 
##' \itemize{
##'   \item{the map of \strong{Plant Functional Group richness} for each selected
##'   simulation year(s), representing the number of PFG present in each pixel
##'   }
##' }
##' 
##' 
##' 
##' @return One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_B \cr PFGrichness}}{to visualize the PFG richness
##'   within the studied area}
##' }
##' 
##' 
##' @keywords FATE, outputs, PFG richness
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##'  
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_mapPFGrichness(name.simulation = "FATE_simulation"
##'                                  , file.simulParam = "Simul_parameters_V1.txt"
##'                                  , year = 850
##'                                  , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_mapPFGrichness(name.simulation = "FATE_simulation"
##'                                  , file.simulParam = "Simul_parameters_V1.txt"
##'                                  , year = c(850, 950)
##'                                  , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom raster raster stack as.data.frame
##' nlayers rasterToPoints writeRaster
##' @importFrom grid unit
##'
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_raster element_blank coord_equal
##' scale_fill_gradientn labs theme element_rect
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom viridis viridis_pal
##' @importFrom grDevices pdf
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_mapPFGrichness = function(
  name.simulation
  , file.simulParam = NULL
  , year
  , opt.no_CPU = 1
  , opt.doPlot = TRUE
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
  #################################################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories -----------------------------------------------------
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs ----------------------------------------------------------
    ## Get PFG names ---------------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask -------------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    ## Get list of arrays and extract years of simulation --------------------------
    years = sort(unique(as.numeric(year)))
    no_years = length(years)
    raster.perPFG.allStrata = grep(paste0("Binary_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.allStrata.BIN), value = TRUE)
    if (length(raster.perPFG.allStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata.BIN, " does not contain adequate files"))
    }
    
    
    ## get the data inside the rasters ---------------------------------------------
    cat("\n GETTING RICHNESS for")
    plot_list = foreach (y = years) %do%
    {
      cat("\n > year", y)
      
      file_name = paste0(dir.output.perPFG.allStrata.BIN
                         , "Binary_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_all.tif")
      
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        ras_TOT = sum(ras)
        ras.pts = as.data.frame(rasterToPoints(ras_TOT))
        colnames(ras.pts) = c("X", "Y", "NB")
        
        output.name = paste0(name.simulation
                             , "/RESULTS/"
                             , basename(dir.save)
                             , "/PFGrichness_YEAR_"
                             , y
                             , "_STRATA_all.tif")
        writeRaster(ras_TOT
                    , filename = output.name
                    , overwrite = TRUE)
        
        message(paste0("\n The output file \n"
                       , " > ", output.name, " \n"
                       , "has been successfully created !\n"))
        
        ## produce the plot ------------------------------------------------------------
        if (opt.doPlot)
        {
          cat("\n PRODUCING PLOT...")
          
          ## Map of PFG richness
          pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "NB")) +
            scale_fill_gradientn("Number of PFG"
                                 , colors = viridis_pal()(max(ras.pts$NB))
                                 , breaks = seq(1, max(ras.pts$NB), 2)) +
            coord_equal() +
            geom_raster() +
            labs(x = "", y = ""
                 , title = paste0("GRAPH D : map of PFG richness - Simulation year : ", y)
                 , subtitle = paste0("For each pixel and stratum, first relative abundances are calculated, "
                                     , "then transformed into binary values :\n"
                                     , "1 if the PFG abundance represents more than 5 % "
                                     , "of the pixel abundance, 0 otherwise.\n"
                                     , "If the PFG is present in one stratum, then it is considered present within the pixel.\n"
                                     , "Finally, simulated PFG occurrences are summed.\n")) +
            .getGraphics_theme() +
            theme(axis.text = element_blank()
                  , legend.key.width = unit(2, "lines"))
          
        } ## END opt.doPlot
        
        return(list(raster = ras_TOT, plot = pp))
      }
    } ## end loop on years
    names(plot_list) = years
    
    ## SAVE plots into file ------------------------------------------------------
    if (opt.doPlot && !is.null(plot_list[[1]]))
    {
      pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_B_map_PFGrichness_", basename(dir.save), ".pdf")
          , width = 12, height = 10)
      for (y in years)
      {
        plot(plot_list[[as.character(y)]][[2]])
      }
      dev.off()
    }
    
    return(plot_list)
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

