### HEADER #####################################################################
##' @title Create maps of both Habitat suitability and simulated occurrences
##' of each Plant Functional Group \cr for one (or several) specific year of a 
##' \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_mapPFGvsHS
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce raster maps of PFG habitat
##' suitability and simulated occurrences for one (or several) specific 
##' \code{FATE-HD} simulation year.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG binary maps
##' @param opt.strata default ALL (\emph{optional}). The stratum number from 
##' which to extract PFG binary maps
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, one preanalytical
##' graphic. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{BIN_perPFG_allStrata} (unless the 
##' \code{opt.strata} is used, then it will be from the folder 
##' \code{BIN_perPFG_perStrata}) and unzipped.
##' Informations extracted lead to the production of one graphic before the
##' maps are compressed again :
##' 
##' \itemize{
##'   \item{the maps of \strong{Plant Functional Group habitat suitability and
##'   occurrences} for each selected simulation year(s), representing the 
##'   probability of presence of each PFG in each pixel compared to its
##'   simulated presence
##'   }
##' }
##' 
##' 
##' 
##' @return One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_C \cr PFGvsHS}}{to visualize the PFG presence
##'   within the studied area (probability and simulated occurrence)}
##' }
##' 
##' 
##' @keywords FATE, outputs, habitat suitability, binary
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
##'                              , file.simulParam = "Simul_parameters_V1.txt"
##'                              , year = 850
##'                              , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
##'                              , file.simulParam = "Simul_parameters_V1.txt"
##'                              , year = c(850, 950)
##'                              , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
##'                              , file.simulParam = "Simul_parameters_V1.txt"
##'                              , year = 850
##'                              , opt.no_CPU = 1
##'                              , opt.strata = 2)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom raster raster stack as.data.frame
##' rasterToPoints
##' @importFrom grid unit
##'
##' @importFrom ggplot2 ggplot aes aes_string facet_wrap
##' geom_raster element_blank coord_equal
##' scale_fill_gradientn labs theme element_rect
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom colorspace heat_hcl
##' @importFrom grDevices pdf
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_mapPFGvsHS = function(
  name.simulation
  , file.simulParam = NULL
  , year
  , opt.no_CPU = 1
  , opt.strata = "all"
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
    
    if (opt.strata == "all")
    {
      dir.output.perPFG.allStrata.BIN = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/")
      .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/"))
    } else
    {
      dir.output.perPFG.allStrata.BIN = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/")
      .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/"))
    }
    
    ## Get number of PFGs ----------------------------------------------------------
    ## Get PFG names ---------------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask -------------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    ## Get raster HS ---------------------------------------------------------------
    file.hs = .getParam(params.lines = abs.simulParam
                        , flag = "PFG_HAB_MASK"
                        , flag.split = "^--.*--$"
                        , is.num = FALSE)
    .testParam_existFile(file.hs)
    
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
    cat("\n GETTING PFG and SDM maps for")
    plot_list = foreach (y = years) %do%
    {
      cat("\n > year", y)
      
      ## SDM maps ------------------------------------------------------------------
      ras.hs = stack(file.hs) * ras.mask
      ras.hs.pts = as.data.frame(rasterToPoints(ras.hs))
      colnames(ras.hs.pts) = c("X", "Y", PFG)
      
      ## PFG maps ------------------------------------------------------------------
      file_name = paste0(dir.output.perPFG.allStrata.BIN
                         , "Binary_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_"
                         , opt.strata
                         , ".tif")
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        ras.pts = as.data.frame(rasterToPoints(ras))
        colnames(ras.pts) = c("X", "Y", gp)
        
        
        ## produce the plot ----------------------------------------------------------
        cat("\n PRODUCING PLOT(S)...")
        plot_list.PFG = foreach(pfg = PFG) %do%
        {
          cat("\n > Preparing for PFG ", pfg)
          
          if (pfg %in% colnames(ras.hs.pts))
          {
            tab = data.frame(ras.hs.pts[, c("X", "Y", pfg)], TYPE = "Habitat Suitability")
            if (pfg %in% colnames(ras.pts))
            {
              tab = rbind(tab, data.frame(ras.pts[, c("X", "Y", pfg)], TYPE = "FATE"))
              
              pp = ggplot(tab, aes_string(x = "X", y = "Y", fill = pfg)) +
                scale_fill_gradientn("Presence probability"
                                     , colors = rev(heat_hcl(9))
                                     , breaks = seq(0, 1, 0.1)) +
                coord_equal() +
                geom_raster() +
                facet_wrap(~ TYPE, ncol = 2) +
                labs(x = "", y = ""
                     , title = paste0("GRAPH C : Habitat suitability vs FATE \n"
                                      , "Simulation year : ", y, " - PFG : ", pfg)
                     , subtitle = paste0("For each pixel and stratum, first relative abundances are calculated, "
                                         , "then transformed into binary values :\n"
                                         , "1 if the PFG abundance represents more than 5 % "
                                         , "of the pixel abundance, 0 otherwise.\n"
                                         , "If the PFG is present in one stratum, then it is considered present within the pixel.\n")) +
                .getGraphics_theme() +
                theme(axis.text = element_blank()
                      , legend.key.width = unit(3, "lines"))
              return(pp)
            }
          }
        } ## END loop on PFG
        names(plot_list.PFG) = PFG
        return(plot_list.PFG)
      }
      
    } ## END loop on years
    names(plot_list) = years
    
    ## SAVE plots into file ------------------------------------------------------
    if (!is.null(plot_list[[1]]))
    {
      pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_B_map_PFGvsHS_", basename(dir.save), ".pdf")
          , width = 10, height = 10)
      for (y in years)
      {
        for (pfg in PFG)
        {
          plot(plot_list[[as.character(y)]][[pfg]])
        }
      }
      dev.off()
    }
    
    return(plot_list)
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

