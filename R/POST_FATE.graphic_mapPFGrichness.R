### HEADER #####################################################################
##' @title Create a map of the Plant Functional Group richness for one (or 
##' several) specific year of a \code{FATE-HD} simulation
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
##' will be used to extract PFG abundance maps
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' @param opt.strata default ALL (\emph{optional}). The stratum number from 
##' which to extract PFG abundance maps
##' 
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, one preanalytical
##' graphic. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{ABUND_perPFG_allStrata} (unless the 
##' \code{opt.strata} is used, then it will be from the folder 
##' \code{ABUND_perPFG_perStrata}) and unzipped.
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
##'   \item{\file{GRAPHIC_C \cr PFGrichness}}{to visualize the PFG richness
##'   within the studied area}
##' }
##' 
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
##'                                     
##' POST_FATE.graphic_mapPFGrichness(name.simulation = "FATE_simulation"
##'                                  , file.simulParam = "Simul_parameters_V1.txt"
##'                                  , year = 850
##'                                  , opt.no_CPU = 1
##'                                  , opt.strata = 2)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom raster raster stack as.data.frame
##' nlayers rasterToPoints
##' @importFrom grid unit
##'
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_raster element_blank coord_equal
##' scale_fill_gradientn labs theme
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
  , opt.strata = "ALL"
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
  
  for (abs.simulParam in abs.simulParams)
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    dir.save = .getParam(params.lines = abs.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/"))
    
    dir.output.perPFG.allStrata = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_perPFG_allStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_perPFG_allStrata/"))
    
    
    ## Get list of arrays and extract years of simulation --------------------------
    years = sort(unique(as.numeric(year)))
    no_years = length(years)
    raster.perPFG.allStrata = grep(paste0("Abund_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.allStrata), value = TRUE)
    if (length(raster.perPFG.allStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata, " does not contain adequate files"))
    }
    
    ## Get number of PFGs ----------------------------------------------------------
    file.globalParam = .getParam(params.lines = abs.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    no_PFG = .getParam(params.lines = file.globalParam
                       , flag = "NB_FG"
                       , flag.split = " "
                       , is.num = TRUE)
    if (length(no_PFG) == 0 || .testParam_notNum(no_PFG))
    {
      stop(paste0("Missing data!\n The number of PFG (NB_FG) within ", file.globalParam, " does not contain any value"))
    }
    
    ## Get PFG names ---------------------------------------------------------------
    PFG = .getParam(params.lines = abs.simulParam
                    , flag = "PFG_LIFE_HISTORY_PARAMS"
                    , flag.split = "^--.*--$"
                    , is.num = FALSE)
    pattern = paste0(name.simulation, "/DATA/PFGS/SUCC/SUCC_")
    PFG = sub(".txt", "", sub(pattern, "", PFG))
    if (length(PFG) != no_PFG)
    {
      stop(paste0("Missing data!\n The number of PFG (NB_FG) within ", file.globalParam
                  , " is different from the number of PFG files contained in ", name.simulation, "/DATA/PFGS/SUCC/"))
    }
    
    ## Get raster mask -------------------------------------------------------------
    file.mask = .getParam(params.lines = abs.simulParam
                          , flag = "MASK"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
    .testParam_existFile(file.mask)
    
    ras.mask = raster(file.mask)
    ras.mask[which(ras.mask[] == 0)] = NA
    ind_1_mask = which(ras.mask[] == 1)
    no_1_mask = length(ind_1_mask)
    
    ## UNZIP the raster saved ------------------------------------------------------
    raster.perPFG.allStrata = foreach(y = years, .combine = "c") %do%
    {
      paste0(dir.output.perPFG.allStrata,
             "Abund_YEAR_",
             y,
             "_",
             PFG,
             "_STRATA_all.tif.gz")
    }
    .unzip(folder_name = dir.output.perPFG.allStrata
           , list_files = raster.perPFG.allStrata
           , nb_cores = opt.no_CPU)
    
    
    ## get the data inside the rasters ---------------------------------------------
    pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_C_map_PFGrichness_", basename(dir.save), ".pdf")
        , width = 10, height = 10)
    cat("\n GETTING RICHNESS for year")
    for (y in years)
    {
      cat(" ", y)
      file_name = paste0(dir.output.perPFG.allStrata,
                         "Abund_YEAR_",
                         y,
                         "_",
                         PFG,
                         "_STRATA_all.tif")
      if (length(which(file.exists(file_name))) == 0)
      {
        stop(paste0("Missing data!\n The names of PFG extracted from files within ", name.simulation, "/DATA/PFGS/SUCC/ : "
                    , paste0("\n", PFG, collapse = "\n")
                    , "\n is different from the files contained in ", dir.output.perPFG.allStrata
                    , "They should be : "
                    , paste0("\n", file_name, collapse = "\n")))
      }
      file_name = file_name[which(file.exists(file_name))]

      ras = stack(file_name) * ras.mask
      ras_TOT = sum(ras)
      ras_REL = ras / ras_TOT
      for (ii in 1:nlayers(ras_REL))
      {
        ras_REL[[ii]][] = ifelse(ras_REL[[ii]][] > 0.05, 1, 0)
      }
      ras_REL = sum(ras_REL)
      ras.pts = as.data.frame(rasterToPoints(ras_REL))
      colnames(ras.pts) = c("X", "Y", "NB")
      
      ## produce the plot ------------------------------------------------------------
      ## Map of PFG richness
      pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "NB")) +
        scale_fill_gradientn("Number of PFG"
                             , colors = viridis_pal()(max(ras.pts$NB))
                             , breaks = seq(1, max(ras.pts$NB), 2)) +
        coord_equal() +
        geom_raster() +
        labs(x = "", y = "", title = paste0("GRAPH C : map of PFG richness - Simulation year : ", y),
             subtitle = paste0("For each pixel, first relative abundances are calculated, ",
                               "then transformed into binary values :\n", 
                               "1 if the PFG abundance represents more than 5 % ",
                               "of the pixel abundance, 0 otherwise.\n",
                               "Finally, simulated PFG occurrences are summed.\n")) +
        theme_fivethirtyeight() +
        theme(axis.text = element_blank()
              , legend.key.width = unit(2, "lines"))
      plot(pp)
    }
    cat("\n")
    dev.off()
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.allStrata, nb_cores = opt.no_CPU)
    
  }
}

