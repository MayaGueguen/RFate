### HEADER #####################################################################
##' @title Create a map of the Plant Functional Group cover 
##'  \cr for one (or several) specific year of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_mapPFGcover
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce a raster map of PFG cover
##' for one (or several) specific \code{FATE-HD} simulation year.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param year an \code{integer} corresponding to the simulation year(s) that 
##' will be used to extract PFG abundance and binary maps
##' @param strata_min an \code{integer} corresponding to the lowest stratum from
##' which PFG abundances are summed up to the highest stratum
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
##' from the results folders \code{ABUND_perPFG_perStrata} and 
##' \code{BIN_perPFG_perStrata} and unzipped.
##' Informations extracted lead to the production of one graphic before the
##' maps are compressed again :
##' 
##' \itemize{
##'   \item{the map of \strong{Plant Functional Group cover} for each selected
##'   simulation year(s), representing the relative cumulated abundance of PFG 
##'   present in each pixel above a height threshold
##'   }
##' }
##' 
##' 
##' 
##' @return One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_D \cr PFGcover}}{to visualize the PFG cover
##'   within the studied area}
##' }
##' 
##'  
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_mapPFGcover(name.simulation = "FATE_simulation"
##'                               , file.simulParam = "Simul_parameters_V1.txt"
##'                               , year = 850
##'                               , strata_min = 3
##'                               , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_mapPFGcover(name.simulation = "FATE_simulation"
##'                               , file.simulParam = "Simul_parameters_V1.txt"
##'                               , year = c(850, 950)
##'                               , strata_min = 3
##'                               , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach
##' @importFrom raster raster stack as.data.frame
##' rasterToPoints
##' @importFrom grid unit
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_raster element_blank coord_equal
##' scale_fill_gradientn labs theme element_rect
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom RColorBrewer brewer.pal
##' @importFrom grDevices pdf
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_mapPFGcover = function(
  name.simulation
  , file.simulParam = NULL
  , year
  , strata_min
  , opt.no_CPU = 1
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
  if (.testParam_notNum(strata_min))
  {
    .stopMessage_beInteger("strata_min")
  }
  #################################################################################################
  
  for (abs.simulParam in abs.simulParams)
  {
    
    cat("\n ############## GRAPHIC POST FATE ############## \n")
    cat("\n Simulation name : ", name.simulation)
    cat("\n Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories -----------------------------------------------------
    dir.save = .getParam(params.lines = abs.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/"))
    
    dir.output.perPFG.perStrata.BIN = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/"))
    
    dir.output.perPFG.perStrata = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_perPFG_perStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_perPFG_perStrata/"))
    
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
    
    ## Get list of arrays and extract years of simulation --------------------------
    years = sort(unique(as.numeric(year)))
    no_years = length(years)
    raster.perPFG.perStrata = grep(paste0("Abund_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.perStrata), value = TRUE)
    
    strata = sapply(sub(".*_STRATA_", "", raster.perPFG.perStrata)
                    , function(x) strsplit(as.character(x), "[.]")[[1]][1])
    strata = sort(unique(as.numeric(strata)))
    no_strata = max(strata)
    if (!(no_strata > 0) || is.infinite(no_strata) | .testParam_notDef(no_strata))
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.perStrata, " does not contain adequate files",
                  " (number of strata null or no strata files found)"))
    }
    if (no_strata < strata_min)
    {
      stop(paste0("Wrong data given!\n `strata_min` is superior to maximum strata found (", no_strata, ")"))
    }
    
    cat("\n Number of strata : ", no_strata)
    cat("\n Selected strata : ", strata_min:no_strata)
    cat("\n")
    
    raster.perPFG.perStrata = raster.perPFG.perStrata[grep(paste0("_STRATA_", strata_min:no_strata, collapse = "|")
                                                           , raster.perPFG.perStrata)]
    if (length(raster.perPFG.perStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.perStrata, " does not contain adequate files"))
    }
    
    ## UNZIP the raster saved ------------------------------------------------------
    combi = expand.grid(year = years, stratum = strata_min:no_strata)
    raster.perPFG.perStrata = foreach(y = combi$year, st = combi$stratum, .combine = "c") %do%
    {
      paste0(dir.output.perPFG.perStrata,
             "Abund_YEAR_",
             y,
             "_",
             PFG,
             "_STRATA_",
             st,
             ".tif.gz")
    }
    .unzip(folder_name = dir.output.perPFG.perStrata
           , list_files = raster.perPFG.perStrata
           , nb_cores = opt.no_CPU)
    
    ## get the data inside the rasters ---------------------------------------------
    pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_E_map_PFGcover_", basename(dir.save), ".pdf")
        , width = 10, height = 10)
    cat("\n GETTING COVER for year")
    for (y in years)
    {
      cat(" ", y)
      
      ## Binary maps
      file_name = paste0(dir.output.perPFG.perStrata.BIN,
                         "Binary_YEAR_",
                         y,
                         "_",
                         PFG)
      file_name = as.vector(sapply(file_name, function(x) paste0(x,
                                                                 "_STRATA_",
                                                                 strata_min:no_strata,
                                                                 ".tif")))
      gp_st = sapply(PFG, function(x) paste0(x, "_STRATA_", strata_min:no_strata))
      gp_st = gp_st[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras.BIN = stack(file_name) * ras.mask
        names(ras.BIN) = gp_st
        
        ## Abundance maps
        file_name = sub("Binary_YEAR_", "Abund_YEAR_", file_name)
        file_name = sub(dir.output.perPFG.perStrata.BIN, dir.output.perPFG.perStrata, file_name)
        
        ras = stack(file_name) * ras.mask
        ras = ras * ras.BIN
        ras_TOT = sum(ras)
        ras_REL = ras_TOT / max(ras_TOT[], na.rm = T)
        ras.pts = as.data.frame(rasterToPoints(ras_REL))
        colnames(ras.pts) = c("X", "Y", "COVER")
        
        ## produce the plot ------------------------------------------------------------
        ## Map of PFG cover
        pp = ggplot(ras.pts, aes_string(x = "X", y = "Y", fill = "COVER")) +
          scale_fill_gradientn("Abundance (%)"
                               , colors = brewer.pal(9, "Greens")
                               , breaks = seq(0, 1, 0.2)
                               , labels = seq(0, 100, 20)) +
          coord_equal() +
          geom_raster() +
          labs(x = "", y = "", title = paste0("GRAPH E : map of PFG cover - Simulation year : ", y),
               subtitle = paste0("For each pixel, PFG abundances from strata ",
                                 strata_min, " to ", no_strata, " are summed,\n",
                                 "then transformed into relative values by dividing by the maximum abundance obtained.\n")) +
          theme_fivethirtyeight() +
          theme(axis.text = element_blank()
                , legend.key.width = unit(2, "lines")
                , panel.background = element_rect(fill = "transparent", colour = NA)
                , plot.background = element_rect(fill = "transparent", colour = NA)
                , legend.background = element_rect(fill = "transparent", colour = NA)
                , legend.box.background = element_rect(fill = "transparent", colour = NA)
                , legend.key = element_rect(fill = "transparent", colour = NA))
        plot(pp)
      }
    } ## end loop on years
    cat("\n")
    dev.off()
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.perStrata, nb_cores = opt.no_CPU)
    
  }
}

