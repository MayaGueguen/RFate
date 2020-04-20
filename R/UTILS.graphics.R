
#################################################################################################
.getGraphics_results = function(name.simulation, abs.simulParam)
{
  ## Get results directories -----------------------------------------------------
  dir.save <<- .getParam(params.lines = abs.simulParam
                         , flag = "SAVING_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
  .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/"))
  
  ## ABUND folders, produced by FATE
  dir.output.perPFG.allStrata <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_perPFG_allStrata/")
  .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_perPFG_allStrata/"))
  
  dir.output.perPFG.perStrata <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_perPFG_perStrata/")
  .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/ABUND_perPFG_perStrata/"))
  
  ## RESOURCES folders, produced by FATE
  dir.output.light <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/LIGHT/")
  .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/LIGHT/"))
  
  dir.output.soil <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/SOIL/")
  .testParam_existFolder(name.simulation, paste0("RESULTS/", basename(dir.save), "/SOIL/"))
  
  ## ABUND REL folder, produced by POST_FATE.relativeAbund function
  dir.output.perPFG.allStrata.REL <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/ABUND_REL_perPFG_allStrata/")
  if (!dir.exists(dir.output.perPFG.allStrata.REL))
  {
    dir.create(path = dir.output.perPFG.allStrata.REL)
  }
  
  ## BINARY folders, produced by POST_FATE.graphic_validationStatistics function
  dir.output.perPFG.allStrata.BIN <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/")
  if (!dir.exists(dir.output.perPFG.allStrata.BIN))
  {
    dir.create(path = dir.output.perPFG.allStrata.BIN)
  }
  dir.output.perPFG.perStrata.BIN <<- paste0(name.simulation, "/RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/")
  if (!dir.exists(dir.output.perPFG.perStrata.BIN))
  {
    dir.create(path = dir.output.perPFG.perStrata.BIN)
  }
}

#################################################################################################
.getGraphics_PFG = function(name.simulation, abs.simulParam)
{
  ## Get number of PFGs ----------------------------------------------------------
  file.globalParam = .getParam(params.lines = abs.simulParam
                               , flag = "GLOBAL_PARAMS"
                               , flag.split = "^--.*--$"
                               , is.num = FALSE)
  no_PFG <<- .getParam(params.lines = paste0(sub(basename(name.simulation), "", name.simulation)
                                             , file.globalParam)
                       , flag = "NO_PFG"
                       , flag.split = " "
                       , is.num = TRUE)
  if (length(no_PFG) == 0 || .testParam_notNum(no_PFG))
  {
    stop(paste0("Missing data!\n The number of PFG (NO_PFG) within ", file.globalParam, " does not contain any value"))
  }
  
  ## Get PFG names ---------------------------------------------------------------
  PFG <<- .getParam(params.lines = abs.simulParam
                    , flag = "PFG_PARAMS_LIFE_HISTORY"
                    , flag.split = "^--.*--$"
                    , is.num = FALSE)
  pattern = ".*SUCC_"
  PFG <<- sub(".txt", "", sub(pattern, "", basename(PFG)))
  if (length(PFG) != no_PFG)
  {
    stop(paste0("Missing data!\n The number of PFG (NO_PFG) within ", file.globalParam
                , " is different from the number of PFG files contained in ", name.simulation, "/DATA/PFGS/SUCC/"))
  }
  
  ## Get MODULES ---------------------------------------------------------------
  no_STRATA <<- .getParam(params.lines = paste0(sub(basename(name.simulation), "", name.simulation)
                                                , file.globalParam)
                          , flag = "NO_STRATA"
                          , flag.split = " "
                          , is.num = TRUE)
  doLight <<- .getParam(params.lines = paste0(sub(basename(name.simulation), "", name.simulation)
                                              , file.globalParam)
                        , flag = "DO_LIGHT_COMPETITION"
                        , flag.split = " "
                        , is.num = TRUE)
  doSoil <<- .getParam(params.lines = paste0(sub(basename(name.simulation), "", name.simulation)
                                             , file.globalParam)
                       , flag = "DO_SOIL_COMPETITION"
                       , flag.split = " "
                       , is.num = TRUE)
}

#################################################################################################
.getGraphics_mask = function(name.simulation, abs.simulParam)
{
  ## Get raster mask -------------------------------------------------------------
  file.mask <<- .getParam(params.lines = abs.simulParam
                          , flag = "MASK"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
  .testParam_existFile(paste0(sub(basename(name.simulation), "", name.simulation)
                              , file.mask))
  
  ras.mask <<- raster(paste0(sub(basename(name.simulation), "", name.simulation)
                             , file.mask))
  ras.mask[which(ras.mask[] == 0)] <<- NA
  ind_1_mask <<- which(ras.mask[] == 1)
  no_1_mask <<- length(ind_1_mask)
  xy.1 <<- xyFromCell(ras.mask, ind_1_mask)
}

#################################################################################################
.getGraphics_theme = function()
{
  return(theme_fivethirtyeight() +
           theme(panel.background = element_rect(fill = "transparent", colour = NA)
                 , plot.background = element_rect(fill = "transparent", colour = NA)
                 , legend.background = element_rect(fill = "transparent", colour = NA)
                 , legend.box.background = element_rect(fill = "transparent", colour = NA)
                 , legend.key = element_rect(fill = "transparent", colour = NA)))
}

