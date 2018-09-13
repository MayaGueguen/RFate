### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG coverage
##' for a \code{FATE-HD} simulation
##' 
##' @name POST_FATE.graphic_evolutionCoverage
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a parameter file containing
##' simulation years at which the \code{FATE-HD} software must save rasters of
##' PFG abundances or simulation objects.
##'              
##' @param name.simulation a \code{string} that corresponds to the main directory
##' or simulation name of the \code{FATE-HD} simulation
##' @param file.simulParam \emph{to be filled}
##' @param opt.no_CPU default 1 (\emph{optional}). The number of resources that 
##' can be used to parallelize the \code{unzip/zip} of raster files
##' 
##' @details 
##' 
##' \emph{to be filled}
##' 
##' 
##' @return T
##' 
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Namespace_constants parameter file
##' PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
##'                                    , global.abund.low = 1000000
##'                                    , global.abund.med = 5000000
##'                                    , global.abund.high = 8000000
##'                                    , global.max.by.cohort = 5000000
##'                                    , global.resource.thresh.med = 13000000
##'                                    , global.resource.thresh.low = 19000000)
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.succ_option = "fateh"
##'                                  , required.no_PFG = 6
##'                                  , required.no_STRATA = 5
##'                                  , required.hs_option = 1
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_duration = c(10,50)
##'                                  , required.simul_duration = 100)
##' 
##' ## Create a SAVE_year_maps or/and SAVE_year_objects parameter file
##' PRE_FATE.params_saveYears(name.simulation = "FATE_simulation"
##'                           , years.maps = c(100, 150, 200)
##'                           , years.objects = 200)
##' 
##' ## Create a Changing_times parameter file
##' PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
##'                               , type.changing = "DIST"
##'                               , mat.changing = data.frame(year = c(50,50,80,80)
##'                                                           , order = c(1,2,1,2)
##'                                                           , file.name = c("MASK_DIST1_50.tif"
##'                                                                           , "MASK_DIST2_50.tif"
##'                                                                           , "MASK_DIST1_80.tif"
##'                                                                           , "MASK_DIST2_80.tif")))
##' 
##' ## Create PFG succession parameter files
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
##'                                                           , type = c("C", "C", "H", "H", "P", "P")
##'                                                           , height = c(10, 250, 36, 68, 1250, 550)
##'                                                           , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                           , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                           , dispersal = 1
##'                                                           , light = c(4, 6, 3, 6, 5, 5)))
##' 
##' ## Create PFG dispersal parameter files
##' PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
##'                              , mat.PFG.disp = data.frame(PFG = paste0("PFG", 1:6)
##'                                                          , d50 = rep(c(500, 500, 100),2)
##'                                                          , d99 = rep(c(10000, 15000, 20000),2)
##'                                                          , ldd = rep(c(100000, 50000, 100000),2)))
##' 
##' ## Create PFG disturbance parameter files
##' PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
##'                                , mat.PFG.dist = data.frame(name = rep(c("DIST1","DIST2"), each = 4)
##'                                                            , responseStage = rep(1:4, 2)
##'                                                            , KilledIndiv_H = c(0,0,0,0,1,1,0,0)
##'                                                            , KilledIndiv_C = c(0,10,10,10,1,1,0,0)
##'                                                            , KilledIndiv_P = c(10,10,10,10,10,0,0,0)
##'                                                            , ResproutIndiv_H = c(0,0,9,10,0,0,5,1)
##'                                                            , ResproutIndiv_C = c(0,0,0,0,0,0,5,1)
##'                                                            , ResproutIndiv_P = c(0,0,0,0,0,0,0,0)))
##' 
##' ## Create a Simulation parameter file
##' file.create("FATE_simulation/DATA/MASK/mask.tif")
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
##'                                 , name.mask = "mask.tif")
##'                                 
##'                                 
##' POST_FATE.graphic_evolutionCoverage(name.simulation
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.no_CPU = 1)
##'                                     
##' 
##' @export
##' 
##' @importFrom raster raster stack
##' @importFrom reshape2 melt
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_line geom_point geom_hline geom_vline geom_label geom_errorbar geom_path
##' element_text element_blank
##' scale_color_discrete scale_color_manual scale_shape_manual facet_grid labs theme
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggrepel geom_label_repel
##'
## END OF HEADER ###############################################################

setwd("~/FATE_Bauges/")
POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_Bauges"
                                    , file.simulParam = "paramSimul_Graz1_CA_rcp26.txt"
                                    , opt.no_CPU = 7)


POST_FATE.graphic_evolutionCoverage = function(
  name.simulation
  , file.simulParam = NULL
  , opt.no_CPU = 1
){
  
  
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  
  if (is.null(file.simulParam))
  {
    abs.simulParams = list.files(paste0(name.simulation, "/PARAM_SIMUL/"))
    abs.simulParams = paste0(name.simulation, "/PARAM_SIMUL/", abs.simulParams)
  } else
  {
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
    .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save)))
    
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
    years = years[round(seq(1, length(years), length.out = 10))]
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
    
    
    ## Get PFG names ---------------------------------------------------------------
    PFG = .getParam(params.lines = abs.simulParam
                    , flag = "PFG_LIFE_HISTORY_PARAMS"
                    , flag.split = "^--.*--$"
                    , is.num = FALSE)
    pattern = paste0(name.simulation, "/DATA/PFGS/SUCC/SUCC_")
    PFG = sub(".txt", "", sub(pattern, "", PFG))
    
    
    ## Get raster mask -------------------------------------------------------------
    file.mask = .getParam(params.lines = abs.simulParam
                          , flag = "MASK"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
    ras.mask = raster(file.mask)
    ras.mask[which(ras.mask[] == 0)] = NA
    ind_1_mask = which(ras.mask[] == 1)
    no_1_mask = length(ind_1_mask)
    
    ## UNZIP the raster saved ------------------------------------------------------
    .unzip(dir.output.perPFG.allStrata, opt.no_CPU)
    
    
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
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      ras = stack(file_name) * ras.mask
      ## calculate the % of cover of each PPFG
      distri[as.character(y), gp] = length(which(ras[ind_1_mask] > 0)) / no_1_mask
      distriAbund[as.character(y), gp] = sum(ras[ind_1_mask], na.rm = T)
    }
    distri.melt = melt(distri)
    colnames(distri.melt) = c("YEAR", "PFG", "Abund")
    distriAbund.melt = melt(distriAbund)
    colnames(distriAbund.melt) = c("YEAR", "PFG", "Abund")
    
    ## produce the plot ------------------------------------------------------------
    
    ## Evolution of space occupation
    pp1 = ggplot(distri.melt, aes_string(x = "YEAR", y = "Abund", group = "PFG")) +
      geom_line() +
      facet_wrap("~ PFG") +
      labs(x = "", y = "", title = paste0("GRAPH A : evolution of species' space occupation"),
           subtitle = paste0("For each PFG, the line represents the evolution through time of its space occupancy,\n",
                             "meaning the percentage of pixels in which the abundance of the species is greater than 0.\n")) +
      theme_fivethirtyeight()
    ggsave(filename = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_A_evolution_spaceOccupancy_", basename(dir.save), ".pdf")
           , plot = pp1, width = 10, height = 8)
    
    ## Evolution of abundance
    pp2 = ggplot(distriAbund.melt, aes_string(x = "YEAR", y = "Abund", group = "PFG")) +
      geom_line() +
      facet_wrap("~ PFG") +
      labs(x = "", y = "", title = paste0("GRAPH B : evolution of species' abundance"),
           subtitle = paste0("For each PFG, the line represents the evolution through time of its abundance\n",
                             "over the whole studied area, meaning the sum of its abundances in every pixel.\n")) +
      theme_fivethirtyeight()
    ggsave(filename = paste0(name.simulation, "/RESULTS/POST_FATE_GRAPHIC_B_evolution_abundance_", basename(dir.save), ".pdf")
           , plot = pp2, width = 10, height = 8)
    
    
    ## ZIP the raster saved ------------------------------------------------------
    .zip(dir.output.perPFG.allStrata, opt.no_CPU)
    
  }
}

