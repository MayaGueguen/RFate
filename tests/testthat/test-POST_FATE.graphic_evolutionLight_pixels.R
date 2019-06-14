library(RFate)
context("POST_FATE.graphic_evolutionLight_pixels() function")


## INPUTS
test_that("POST_FATE.graphic_evolutionLight_pixels gives error with missing data", {
  expect_error(POST_FATE.graphic_evolutionLight_pixels()
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(NA)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(NULL)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionLight_pixels gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_evolutionLight_pixels(1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionLight_pixels("a")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(factor("A"))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(data.frame(1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionLight_pixels gives error with wrong data : file.simulParam", {
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = NULL)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "toto")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionLight_pixels gives error with wrong data : files", {
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)  
  cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("SAVE_DIR\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--SAVE_DIR--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (SAVE_DIR) does not contain any value"
               , fixed = TRUE)
  cat("--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/")
  
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                       , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_allStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                       , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")
  
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/LIGHT/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/LIGHT/")

  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--MASK--\nFATE_simulation/Mask.asc\n--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Wrong name file given!\n `FATE_simulation/Mask.asc` does not exist"
               , fixed = TRUE)
  
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/Mask.asc")
  expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
                                                          , file.simulParam = "ParamSimul.txt")
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/LIGHT/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/Hello/LIGHT/Light_Resources_YEAR_1_STRATA_1.tif")
})


## INPUTS
test_that("POST_FATE.graphic_evolutionLight_pixels gives error with wrong data : rasters", {
  # expect_error(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
  #                                                         , file.simulParam = "ParamSimul.txt")
  #              # , "objet 'distri' introuvable"
  #              # , "Cannot create RasterLayer object from this file"
  #              , "attempt to set 'colnames' on an object with less than two dimensions"
  #              , fixed = TRUE)
  
  # expect_warning(POST_FATE.graphic_evolutionLight_pixels(name.simulation = "FATE_simulation"
  #                                                           , file.simulParam = "ParamSimul.txt"
  #                                                           , opt.cells_ID = c(1, 2))
  #                , "The values given in `opt.cells_ID` do not match with any cells of the studied area"
  #                , fixed = TRUE)
})

