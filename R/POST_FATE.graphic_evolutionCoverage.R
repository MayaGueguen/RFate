### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG coverage
##' and abundance through time for a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_evolutionCoverage
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce two graphical 
##' representations for a \code{FATE-HD} simulation : 1) the evolution through
##' time of the space occupation of each PFG ; 2) the evolution through time
##' of the abundance of each PFG.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam a \code{string} that corresponds to the name of a
##' parameter file that will be contained into the \code{PARAM_SIMUL} folder
##' of the \code{FATE-HD} simulation
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the
##' ordinate scale will be adapted for each PFG for the graphical representation
##' of the  evolution of abundances through time
##' @param opt.doPlot default TRUE (\emph{optional}). If TRUE, plot(s) will be
##' processed, otherwise only the calculation and reorganization of outputs
##' will occur, be saved and returned.
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, two preanalytical
##' graphics : 
##' 
##' \itemize{
##'   \item{the evolution of \strong{space occupancy} of each Plant Functional
##'   Group through simulation time, with \emph{space occupancy} representing
##'   the percentage of pixels within the mask of studied area where the PFG
##'   is present
##'   }
##'   \item{the evolution of \strong{abundance} of each Plant Functional
##'   Group through simulation time, with \emph{abundance} being the sum over
##'   the whole studied area of the PFG abundances (\code{FATE-HD} 
##'   \emph{arbitrary unit})
##'   }
##' }
##' 
##' It requires that the \code{\link{POST_FATE.temporalEvolution}} has been run 
##' and that the \code{POST_FATE.evolution_abundance_PIXEL_[...].csv} exists.
##' 
##' If the information has been provided, the graphics will be also
##' done per habitat.
##' 
##' 
##' 
##' @return Two \code{POST_FATE_[...].pdf} files are created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr spaceOccupancy}}{to visualize for each PFG the
##'   evolution of its occupation of the studied area through simulation time}
##'   \item{\file{GRAPHIC_A \cr abundance}}{to visualize for each PFG the
##'   evolution of its abundance within the whole studied area through
##'   simulation time}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time
##' 
##' @seealso \code{\link{POST_FATE.temporalEvolution}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.no_CPU = 4)
##'                                     
##' POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.abund_fixedScale = FALSE
##'                                     , opt.no_CPU = 1)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom data.table fread
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_hline geom_vline geom_label geom_errorbar geom_path
##' element_text element_blank element_rect
##' scale_color_discrete scale_color_manual scale_shape_manual facet_grid labs theme
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggrepel geom_label_repel
##'
## END OF HEADER ###############################################################



POST_FATE.graphic_evolutionCoverage = function(
  name.simulation
  , file.simulParam = NULL
  , opt.abund_fixedScale = TRUE
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
    
    years = colnames(tab.abundance)
    years = years[which(!(years %in% c("PFG", "ID", "X", "Y", "HAB")))]
    years = as.numeric(years)
    
    hab_names = unique(tab.abundance$HAB)
    no_hab = length(hab_names)
    
    ## Transform the data inside the table -----------------------------------------
    cat("\n GETTING COVERAGE and ABUNDANCE over the whole area...")
    
    tab.abundance.split = split(tab.abundance, list(tab.abundance$PFG, tab.abundance$HAB))
    distriAbund.melt = foreach(i = 1:length(tab.abundance.split), .combine = "rbind") %do%
      {
        pfg = strsplit(names(tab.abundance.split)[i], "[.]")[[1]][1]
        hab = strsplit(names(tab.abundance.split)[i], "[.]")[[1]][2]
        tab = tab.abundance.split[[i]]
        tab = tab[, as.character(years)]
        
        return(data.frame(PFG = pfg, HAB = hab, YEAR = years
                          , Abund = colSums(tab, na.rm = TRUE)
                          , stringsAsFactors = FALSE))
      }
    
    distri.melt = foreach(i = 1:length(tab.abundance.split), .combine = "rbind") %do%
      {
        pfg = strsplit(names(tab.abundance.split)[i], "[.]")[[1]][1]
        hab = strsplit(names(tab.abundance.split)[i], "[.]")[[1]][2]
        tab = tab.abundance.split[[i]]
        tab = tab[, as.character(years)]
        tab = as.matrix(tab)
        tab = apply(tab, 2, function(x) length(which(x > 0)))
        
        return(data.frame(PFG = pfg, HAB = hab, YEAR = years
                          , Abund = tab / no_1_mask
                          , stringsAsFactors = FALSE))
      }
    cat("\n")
    
    write.csv(distri.melt
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_evolution_spaceOccupancy_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = TRUE)
    
    write.csv(distriAbund.melt
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_evolution_abundance_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = TRUE)
    
    message(paste0("\n The output files \n"
                   , " > POST_FATE_evolution_spaceOccupancy_"
                   , basename(dir.save)
                   , ".csv \n"
                   , " > POST_FATE_evolution_abundance_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "have been successfully created !\n"))

    
    ## produce the plot ------------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n PRODUCING PLOT(S)...")
      col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
      col_fun = colorRampPalette(col_vec)
      
      ## Evolution of space occupation
      pp1 = ggplot(distri.melt, aes_string(x = "YEAR", y = "Abund * 100", color = "factor(HAB)")) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG") +
        scale_color_manual("Habitat", values = col_fun(no_hab)) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
                               "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
        .getGraphics_theme()
      ggsave(filename = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_A_evolution_spaceOccupancy_", basename(dir.save), ".pdf")
             , plot = pp1, width = 10, height = 8)
      
      ## Evolution of abundance
      pp2 = ggplot(distriAbund.melt, aes_string(x = "YEAR", y = "Abund", color = "HAB")) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
        scale_color_manual("Habitat", values = col_fun(no_hab)) +
        labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
             subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                               "over the whole studied area, meaning the sum of its abundances in every pixel.\n")) +
        .getGraphics_theme()
      ggsave(filename = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_A_evolution_abundance_", basename(dir.save), ".pdf")
             , plot = pp2, width = 10, height = 8)
    } ## END opt.doPlot
    
    
    cat("\n> Done!\n")
    cat("\n")
    
    return(list(tab.spaceOccupancy = distri.melt
                , tab.abundance = distriAbund.melt
                , graph.spaceOccupancy = pp1
                , graph.abundance = pp2))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

