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
##' @param opt.abund_fixedScale default \code{TRUE}. If \code{FALSE}, the ordinate
##' scale will be adapted for each PFG for the graphical representation of the 
##' evolution of abundances through time
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' 
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
##' @seealso \code{\link{POST_FATE.relativeAbund_presenceAbsence}}
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
    .unzip_ALL(folder_name = dir.output.perPFG.allStrata, nb_cores = opt.no_CPU)
    
    
    ## get the data inside the rasters ---------------------------------------------
    distri = distriAbund = array(0,
                                 dim = c(no_years, no_PFG),
                                 dimnames = list(years, PFG))
    cat("\n GETTING COVERAGE for year")
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
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]

      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        ras = as.data.frame(ras)
        
        ## calculate the % of cover of each PPFG
        distri[as.character(y), gp] = apply(ras, 2, function(x) length(which(x[ind_1_mask] > 0)) / no_1_mask)
        distriAbund[as.character(y), gp] = apply(ras, 2, function(x) sum(x[ind_1_mask], na.rm = T))
      }
    } ## end loop on years
    cat("\n")
    
    distri.melt = melt(distri)
    colnames(distri.melt) = c("YEAR", "PFG", "Abund")
    distriAbund.melt = melt(distriAbund)
    colnames(distriAbund.melt) = c("YEAR", "PFG", "Abund")
    
    ## produce the plot ------------------------------------------------------------
    
    ## Evolution of space occupation
    pp1 = ggplot(distri.melt, aes_string(x = "YEAR", y = "Abund * 100", group = "PFG")) +
      geom_line() +
      facet_wrap("~ PFG") +
      labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
           subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
                             "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    ggsave(filename = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_A_evolution_spaceOccupancy_", basename(dir.save), ".pdf")
           , plot = pp1, width = 10, height = 8)
    
    ## Evolution of abundance
    pp2 = ggplot(distriAbund.melt, aes_string(x = "YEAR", y = "Abund", group = "PFG")) +
      geom_line() +
      facet_wrap("~ PFG", scales = ifelse(opt.abund_fixedScale, "fixed", "free_y")) +
      labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' abundance"),
           subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                             "over the whole studied area, meaning the sum of its abundances in every pixel.\n")) +
      theme_fivethirtyeight() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA)
            , plot.background = element_rect(fill = "transparent", colour = NA)
            , legend.background = element_rect(fill = "transparent", colour = NA)
            , legend.box.background = element_rect(fill = "transparent", colour = NA)
            , legend.key = element_rect(fill = "transparent", colour = NA))
    ggsave(filename = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_A_evolution_abundance_", basename(dir.save), ".pdf")
           , plot = pp2, width = 10, height = 8)
    
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(folder_name = dir.output.perPFG.allStrata, nb_cores= opt.no_CPU)
    
    write.csv(distri
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_evolution_spaceOccupancy_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = TRUE)
    
    write.csv(distriAbund
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
    
  }
}

