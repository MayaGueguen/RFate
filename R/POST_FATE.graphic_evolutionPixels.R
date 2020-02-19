### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG abundance
##' through time for 5 (or more) pixels of a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_evolutionPixels
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
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the ordinate
##' scale will be adapted for each PFG for the graphical representation of the 
##' evolution of abundances through time
##' @param opt.cells_ID default NULL (\emph{optional}). The cells ID of the 
##' studied area for which PFG abundances will be extracted.
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
##' POST_FATE.graphic_evolutionPixels(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt")
##'                                     
##' POST_FATE.graphic_evolutionPixels(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.abund_fixedScale = FALSE)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom grDevices colorRampPalette
##' @importFrom reshape2 melt
##' @importFrom foreach foreach
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_hline geom_vline geom_label 
##' geom_errorbar geom_path geom_area
##' element_text element_blank element_rect
##' scale_color_discrete scale_color_manual 
##' scale_shape_manual scale_fill_manual
##' facet_grid labs theme
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggrepel geom_label_repel
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_evolutionPixels = function(
  name.simulation
  , file.simulParam = NULL
  , opt.abund_fixedScale = TRUE
  , opt.cells_ID = NULL
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

    ## Get the abundance table -----------------------------------------------------
    file.abundance = paste0(name.simulation
                            , "/RESULTS/POST_FATE_evolution_abundance_PIXEL_"
                            , basename(dir.save)
                            , ".csv")
    .testParam_existFile(file.abundance)
    tab.abundance = fread(file.abundance)
    tab.abundance = as.data.frame(tab.abundance)
    tab.abundance$TYPE = "abundance"
    colnames(tab.abundance)[which(colnames(tab.abundance) == "PFG")] = "GROUP"
    
    years = colnames(tab.abundance)
    years = years[which(!(years %in% c("TYPE", "GROUP", "ID", "X", "Y", "HAB")))]
    years = as.numeric(years)
    no_years = length(years)
    
    strata = paste0("Stratum ", (no_STRATA - 1):0)
    
    ## Get resources tables --------------------------------------------------------
    if (doLight)
    {
      file.light = paste0(name.simulation
                              , "/RESULTS/POST_FATE_evolution_light_PIXEL_"
                              , basename(dir.save)
                              , ".csv")
      .testParam_existFile(file.light)
      tab.light = fread(file.light)
      tab.light = as.data.frame(tab.light)
      tab.light$STRATUM = paste0("Stratum ", tab.light$STRATUM)
      tab.light$TYPE = "light"
      colnames(tab.light)[which(colnames(tab.light) == "STRATUM")] = "GROUP"
      
      tab.abundance = rbind(tab.abundance, tab.light)
    }
    
    if (doSoil)
    {
      file.soil = paste0(name.simulation
                          , "/RESULTS/POST_FATE_evolution_soil_PIXEL_"
                          , basename(dir.save)
                          , ".csv")
      .testParam_existFile(file.soil)
      tab.soil = fread(file.soil)
      tab.soil = as.data.frame(tab.soil)
      tab.soil$GROUP = "soil"
      tab.soil$TYPE = "soil"
      
      tab.abundance = rbind(tab.abundance, tab.soil)
    }

    
    ## Get concerned cells id ------------------------------------------------------
    IDS = sample(unique(tab.abundance$ID), 5)
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
    
    cat("\n Number of years : ", no_years)
    cat("\n Selected years : ", years)
    cat("\n Selected cells : ", IDS)
    cat("\n")
    
    ## Transform the data inside the table -----------------------------------------
    distriAbund = tab.abundance[which(tab.abundance$ID %in% IDS), , drop = FALSE]
    distriAbund = distriAbund[, c("TYPE", "GROUP", "ID", "HAB", as.character(years))]
    distriAbund = melt(distriAbund, id.vars = c("TYPE", "GROUP", "ID", "HAB"))
    colnames(distriAbund) = c("TYPE", "GROUP", "ID", "HAB", "YEAR", "value")
    distriAbund$YEAR = as.numeric(as.character(distriAbund$YEAR))
    distriAbund$TYPE = factor(distriAbund$TYPE, c("light", "abundance", "soil"))
    distriAbund$GROUP = factor(distriAbund$GROUP, c(strata, PFG, "soil"))
    
    write.csv(distriAbund
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_evolution_pixels_"
                              , ifelse(length(IDS) <= 5, paste0(IDS, collapse = "_"), length(IDS))
                              , "_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    message(paste0("\n The output file \n"
                   , " > POST_FATE_evolution_pixels_"
                   , ifelse(length(IDS) <= 5, paste0(IDS, collapse = "_"), length(IDS))
                   , "_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "has been successfully created !\n"))
    
    ## produce the plot ------------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n PRODUCING PLOT...")
      vec_col1 = c('#0077BB', '#33BBEE', '#009988', '#EE7733', '#CC3311', '#EE3377')
      val_col1 = c(rep(rgb(1,1,1,1), no_STRATA)
                  , colorRampPalette(vec_col1)(no_PFG)
                  , "grey30")
      names(val_col1) = c(strata, PFG, "soil")
      
      vec_col2 = c('#FEC44F', '#FB9A29', '#EC7014', '#CC4C02', '#993404', '#662506')
      val_col2 = colorRampPalette(vec_col2)(no_STRATA)
      names(val_col2) = strata
      
      
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "value")) +
        geom_line(data = distriAbund[which(distriAbund$TYPE == "soil"),]
                  , color = "grey30"
                  , lwd = 0.7) +
        geom_line(data = distriAbund[which(distriAbund$TYPE == "abundance"),]
                  , aes_string(color = "GROUP")
                  , lwd = 0.7) +
        scale_color_manual("", values = val_col1) +
        geom_area(data = distriAbund[which(distriAbund$TYPE == "light"),]
                  , aes_string(fill = "GROUP")
                  , position = "identity", alpha= 0.4) +
        scale_fill_manual("", values = val_col2) +
        facet_grid("TYPE ~ ID", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                               "for 5 randomly selected pixels within the studied area.\n")) +
        .getGraphics_theme()
      
      ggsave(filename = paste0(name.simulation
                               , "/RESULTS/POST_FATE_GRAPHIC_evolution_pixels_"
                               , ifelse(length(IDS) <= 5, paste0(IDS, collapse = "_"), length(IDS))
                               , "_"
                               , basename(dir.save)
                               , ".pdf")
             , plot = pp, width = 10, height = 8)
    } ## END opt.doPlot
    
    cat("\n> Done!\n")
    cat("\n")
    
    return(list(tab = distriAbund, plot = pp))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

