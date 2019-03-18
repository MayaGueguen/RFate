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
##' @seealso \code{\link{POST_FATE.relativeAbund_presenceAbsence}}
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
    
    ## Get results directories -----------------------------------------------------
    dir.save = .getParam(params.lines = abs.simulParam
                         , flag = "SAVE_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/"))
    
    dir.output.perPFG.allStrata.BIN = paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/")
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/"))
    
    
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
    pattern = ".*SUCC_"
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
    raster.perPFG.allStrata = grep(paste0("Binary_YEAR_", years, "_", collapse = "|")
                                   , list.files(dir.output.perPFG.allStrata.BIN), value = TRUE)
    if (length(raster.perPFG.allStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata.BIN, " does not contain adequate files"))
    }
    
    
    ## get the data inside the rasters ---------------------------------------------
    pdf(file = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_B_map_PFGrichness_", basename(dir.save), ".pdf")
        , width = 10, height = 10)
    
    cat("\n GETTING RICHNESS for year")
    for (y in years)
    {
      cat(" ", y)
      
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
                             , "_STRATA_"
                             , opt.strata
                             , ".tif")
        writeRaster(ras_TOT
                    , filename = output.name
                    , overwrite = TRUE)
        
        message(paste0("\n The output file \n"
                       , " > ", output.name, " \n"
                       , "has been successfully created !\n"))
        
        ## produce the plot ------------------------------------------------------------
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
    
  }
}

