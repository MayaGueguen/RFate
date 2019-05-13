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
##' @param no.years an \code{integer} corresponding to the number of simulation
##' years that will be used to extract PFG abundance maps
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the
##' ordinate scale will be adapted for each PFG for the graphical representation
##' of the  evolution of abundances through time
##' @param opt.ras_habitat default NULL (\emph{optional}). A \code{string} that
##' corresponds to the file name of a raster mask, with an \code{integer} value
##' within each pixel, corresponding to a specific habitat
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' @param opt.doPlot default TRUE (\emph{optional}). If TRUE, plot(s) will be
##' processed, otherwise only the calculation and reorganization of outputs
##' will occur, be saved and returned.
##' 
##' @details 
##' 
##' This function allows one to obtain, for a specific \code{FATE-HD} simulation
##' and a specific parameter file within this simulation, two preanalytical
##' graphics. \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved
##' from the results folder \code{ABUND_perPFG_allStrata} and unzipped.
##' Informations extracted lead to the production of two graphics before the
##' maps are compressed again :
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
##' If a raster mask for habitat has been provided, the graphics will be also
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
##' @seealso \code{\link{POST_FATE.relativeAbund}}
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
##'                                     , no.years = 50
##'                                     , opt.no_CPU = 4)
##'                                     
##' POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
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
##' @importFrom raster raster stack as.data.frame
##' @importFrom reshape2 melt
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
  , no.years = 10
  , opt.abund_fixedScale = TRUE
  , opt.ras_habitat = NULL
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
  if (!.testParam_notDef(opt.ras_habitat))
  {
    if (nchar(opt.ras_habitat) > 0)
    {
      .testParam_existFile(opt.ras_habitat)
      ras.habitat = raster(opt.ras_habitat)
    }
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
    .getGraphics_mask(abs.simulParam = abs.simulParam)
    
    ## Get habitat information -----------------------------------------------------
    no_hab = 1
    hab_names = "ALL"
    if (exists("ras.habitat"))
    {
      ras.habitat = ras.habitat * ras.mask
      df.habitat = data.frame(ID = cellFromXY(ras.habitat, xy.1))
      df.habitat$HAB = ras.habitat[df.habitat$ID]
      hab_names = c(hab_names, unique(df.habitat$HAB))
      hab_names = hab_names[which(!is.na(hab_names))]
      no_hab = length(hab_names)
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
    
    
    ## get the data inside the rasters ---------------------------------------------
    distri = distriAbund = array(0,
                                 dim = c(no_years, no_PFG, no_hab),
                                 dimnames = list(years, PFG, hab_names))
    cat("\n GETTING COVERAGE for year")
    for (y in years)
    {
      cat(" ", y)
      file_name = paste0(dir.output.perPFG.allStrata,
                         "Abund_YEAR_",
                         y,
                         "_",
                         PFG,
                         "_STRATA_all")
      if (length(which(file.exists(paste0(file_name, ".tif")))) > 0)
      {
        file_name = paste0(file_name, ".tif")
      } else if (length(which(file.exists(paste0(file_name, ".img")))) > 0)
      {
        file_name = paste0(file_name, ".img")
      } else if (length(which(file.exists(paste0(file_name, ".asc")))) > 0)
      {
        file_name = paste0(file_name, ".asc")
      }
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
        ras = as.data.frame(ras)
        
        if (exists("df.habitat"))
        {
          ras = merge(ras, df.habitat, by.x = "row.names", by.y = "ID", all.x = TRUE)
        } else
        {
          ras$HAB = "ALL"
        }
        ras.split = split(ras, ras$HAB)
        
        for (habi in hab_names)
        {
          if (habi == "ALL")
          {
            tmp = ras
          } else
          {
            tmp = ras.split[[as.character(habi)]]
          }
          tmp = tmp[, -which(colnames(tmp) %in% c("Row.names", "HAB")), drop = FALSE]
          
          if (nrow(tmp) > 0)
          {
            ## calculate the % of cover of each PPFG
            distri[as.character(y), gp, as.character(habi)] = apply(tmp, 2, function(x) length(which(x[ind_1_mask] > 0)) / no_1_mask)
            distriAbund[as.character(y), gp, as.character(habi)] = apply(tmp, 2, function(x) sum(x[ind_1_mask], na.rm = T))
          }
        }
      }
    } ## END loop on years
    cat("\n")
    
    distri.melt = melt(distri)
    colnames(distri.melt) = c("YEAR", "PFG", "HAB", "Abund")
    distriAbund.melt = melt(distriAbund)
    colnames(distriAbund.melt) = c("YEAR", "PFG", "HAB", "Abund")
    
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
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.allStrata, nb_cores= opt.no_CPU)
    
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
    
    cat("\n> Done!\n")
    cat("\n")
    
    message(paste0("\n The output files \n"
                   , " > POST_FATE_evolution_spaceOccupancy_"
                   , basename(dir.save)
                   , ".csv \n"
                   , " > POST_FATE_evolution_abundance_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "have been successfully created !\n"))
    
    return(list(tab.spaceOccupancy = distri.melt
                , tab.abundance = distriAbund.melt
                , graph.spaceOccupancy = pp1
                , graph.abundance = pp2))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

