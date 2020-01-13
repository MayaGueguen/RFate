library(RFate)
library(raster)
context("POST_FATE.graphic_evolutionSoil_pixels() function")


## INPUTS
test_that("POST_FATE.graphic_evolutionSoil_pixels gives error with missing data", {
  expect_error(POST_FATE.graphic_evolutionSoil_pixels()
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(NA)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(NULL)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionSoil_pixels gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels("a")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(factor("A"))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(data.frame(1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionSoil_pixels gives error with wrong data : file.simulParam", {
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = NULL)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "toto")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionSoil_pixels gives error with wrong data : files", {
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)  
  cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("SAVE_DIR\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--SAVE_DIR--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (SAVE_DIR) does not contain any value"
               , fixed = TRUE)
  cat("--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/")
  
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_allStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")
  
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/SOIL/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/SOIL/")
  
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--MASK--\nFATE_simulation/Mask.tif\n--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong name file given!\n `FATE_simulation/Mask.tif` does not exist"
               , fixed = TRUE)
  
  PNE_PARAM = .loadData("PNE_PARAM")
  writeRaster(PNE_PARAM$masks$maskEcrins, filename = "FATE_simulation/Mask.tif", overwrite = TRUE)
  
  expect_error(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/SOIL/ does not contain adequate files")
})


## INPUTS
test_that("POST_FATE.graphic_evolutionSoil_pixels gives error with wrong data : rasters", {
  
  PNE_RESULTS = .loadData("PNE_RESULTS")
  PFG.names = names(PNE_RESULTS$abund_str.equilibrium)
  PFG.names = sub("PNE_year_800_", "", PFG.names)
  PFG.names = sapply(PFG.names, function(x) strsplit(x, "_")[[1]][1])
  for (pfg in PFG.names[1])
  {
    ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
    stk = PNE_RESULTS$abund_str.equilibrium[[ind]]
    ras = sum(stk)
    writeRaster(ras
                , filename = paste0("FATE_simulation/RESULTS/Hello/SOIL/Soil_Resources_YEAR_1.tif")
                , overwrite = TRUE)
  }
  
  expect_message(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                        , file.simulParam = "ParamSimul.txt")
                 , "has been successfully created !"
                 , fixed = TRUE)
  
  expect_warning(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                        , file.simulParam = "ParamSimul.txt"
                                                        , opt.cells_ID = c(1, 2))
                 , "The values given in `opt.cells_ID` do not match with any cells of the studied area"
                 , fixed = TRUE)
  
  expect_output(str(POST_FATE.graphic_evolutionSoil_pixels(name.simulation = "FATE_simulation"
                                                           , file.simulParam = "ParamSimul.txt"
                                                           , opt.cells_ID = c(254, 262, 871))), "list")
})

