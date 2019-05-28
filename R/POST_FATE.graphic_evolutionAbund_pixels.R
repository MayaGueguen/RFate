### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG abundance
##' through time for 5 (or more) pixels of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_evolutionAbund_pixels
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce one graphical representation
##' for a \code{FATE-HD} simulation : the evolution through time of the 
##' abundance of each PFG for 5 (or more) randomly selected cells of the
##' studied area.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param no.years an \code{integer} corresponding to the number of simulation
##' years that will be used to extract PFG abundance maps
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the ordinate
##' scale will be adapted for each PFG for the graphical representation of the 
##' evolution of abundances through time
##' @param opt.cells_ID default NULL (\emph{optional}). The cells ID of the 
##' studied area for which PFG abundances will be extracted.
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' @param opt.doPlot default TRUE (\emph{optional}). If TRUE, plot(s) will be
##' processed, otherwise only the calculation and reorganization of outputs
##' will occur, be saved and returned.
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, one preanalytical
##' graphics. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{ABUND_perPFG_allStrata} and unzipped.
##' Informations extracted lead to the production of one graphic before the
##' maps are compressed again :
##' 
##' \itemize{
##'   \item{the evolution of \strong{abundance} of each Plant Functional
##'   Group through simulation time, within 5 (or more) randomly selected pixels
##'   of the studied area (\code{FATE-HD} \emph{arbitrary unit})
##'   }
##' }
##' 
##' 
##' 
##' @return One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr abundance}}{to visualize for each PFG the
##'   evolution of its abundance within each selected pixel through
##'   simulation time}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_evolutionAbund_pixels(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_evolutionAbund_pixels(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , no.years = 50
##'                                     , opt.no_CPU = 4)
##'                                     
##' POST_FATE.graphic_evolutionAbund_pixels(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , no.years = 50
##'                                     , opt.abund_fixedScale = FALSE
##'                                     , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom grDevices colorRampPalette
##' @importFrom raster raster stack as.data.frame rasterToPoints
##' @importFrom reshape2 melt
##' @importFrom foreach foreach
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_hline geom_vline geom_label geom_errorbar geom_path
##' element_text element_blank element_rect
##' scale_color_discrete scale_color_manual scale_shape_manual facet_grid labs theme
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggrepel geom_label_repel
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_evolutionAbund_pixels = function(
  name.simulation
  , file.simulParam = NULL
  , no.years = 10
  , opt.abund_fixedScale = TRUE
  , opt.cells_ID = NULL
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
    
    ## Get concerned cells id ------------------------------------------------------
    IDS = sample(ind_1_mask, 5)
    if (!is.null(opt.cells_ID))
    {
      if (sum(opt.cells_ID %in% ind_1_mask) == length(opt.cells_ID))
      {
        IDS = opt.cells_ID
      } else
      {
        warning(paste0("The values given in `opt.cells_ID` do not match with any cells of the studied area \n"
                       , "(obtained from the raster file `"
                       , file.mask
                       , "`)\n"
                       , "They will be replaced by randomly selected cells."))
      }
    }
    
    ## Get list of arrays and extract years of simulation --------------------------
    raster.perPFG.allStrata = grep("Abund_", list.files(dir.output.perPFG.allStrata), value = TRUE)
    if (length(raster.perPFG.allStrata) == 0)
    {
      stop(paste0("Missing data!\n The folder ", dir.output.perPFG.allStrata, " does not contain adequate files"))
    }
    years = sapply(sub("Abund_YEAR_", "", raster.perPFG.allStrata)
                   , function(x) strsplit(as.character(x), "_")[[1]][1])
    years = sort(unique(as.numeric(years)))
    years = years[round(seq(1, length(years), length.out = min(no.years, length(years))))]
    no_years = length(years)
    
    ## UNZIP the raster saved ------------------------------------------------------
    .unzip_ALL(folder_name = dir.output.perPFG.allStrata, nb_cores = opt.no_CPU)
    
    cat("\n Number of years : ", no_years)
    cat("\n Selected years : ", years)
    cat("\n Selected cells : ", IDS)
    cat("\n")
    
    
    ## get the data inside the rasters ---------------------------------------------
    cat("\n GETTING ABUNDANCE for year")
    distriAbund = foreach (y = years, .combine = "rbind") %do%
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
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        res = as.data.frame(ras)
        res = res[IDS, , drop = FALSE]
        colnames(res) = gp
        res$ID = IDS
        res = melt(res, id.vars = "ID")
        colnames(res) = c("ID", "PFG", "Abund")
        res$TYPE = sub("[0123456789]", "", res$PFG)
        return(data.frame(YEAR = y, res))
      }
    }
    cat("\n")
    
    
    ## produce the plot ------------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n PRODUCING PLOT...")
      vec_col = c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c'
                  , '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00'
                  , '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')
      fun_col = colorRampPalette(vec_col)
      
      ## Evolution of abundance
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "Abund", color = "PFG")) +
        scale_color_manual("", values = fun_col(no_PFG)) +
        geom_line() + ## lwd = 0.8 ?
        facet_grid("TYPE ~ ID", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                               "for 5 randomly selected pixels within the studied area.\n")) +
        .getGraphics_theme()
      # xlim(0, 400) + ??
      
      ggsave(filename = paste0(name.simulation
                               , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_abundance_pixels_"
                               , ifelse(length(IDS) <= 5, paste0(IDS, collapse = "_"), length(IDS))
                               , "_"
                               , basename(dir.save)
                               , ".pdf")
             , plot = pp, width = 10, height = 8)
    } ## END opt.doPlot
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip_ALL(folder_name = dir.output.perPFG.allStrata, nb_cores= opt.no_CPU)
    
    write.csv(distriAbund
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_evolution_abundance_pixels_"
                              , ifelse(length(IDS) <= 5, paste0(IDS, collapse = "_"), length(IDS))
                              , "_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    cat("\n> Done!\n")
    cat("\n")
    
    message(paste0("\n The output file \n"
                   , " > POST_FATE_evolution_abundance_pixels_"
                   , ifelse(length(IDS) <= 5, paste0(IDS, collapse = "_"), length(IDS))
                   , "_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "has been successfully created !\n"))
    
    return(list(tab = distriAbund, plot = pp))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

