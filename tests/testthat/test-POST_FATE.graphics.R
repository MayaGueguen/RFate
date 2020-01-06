library(RFate)
context("POST_FATE.graphics() function")


## INPUTS
test_that("POST_FATE.graphics gives error with missing data", {
  expect_error(POST_FATE.graphics()
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(NA)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(NULL)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphics(1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics("a")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(factor("A"))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphics(data.frame(1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : file.simulParam", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = NULL)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "toto")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : files", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)  
  cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")

  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  
  if (file.exists("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")) file.remove("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  cat("GLOBAL_PARAMS\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                                   , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)

  cat("--GLOBAL_PARAMS--\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong name file given!\n `FATE_simulation/GlobalParam.txt` does not exist"
               , fixed = TRUE)
  
  file.create("FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("HOP \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (DO_LIGHT_COMPETITION) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("DO_LIGHT_COMPETITION \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (DO_SOIL_COMPETITION) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("DO_SOIL_COMPETITION \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (DO_LIGHT_COMPETITION) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("DO_LIGHT_COMPETITION 1\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `flag` (DO_SOIL_COMPETITION) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("DO_LIGHT_COMPETITION 1\nDO_SOIL_COMPETITION 1\n", file = "FATE_simulation/GlobalParam.txt")
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : year", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `year` must be an integer > 0"
               , fixed = TRUE)
  
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = "a")
               , "Wrong type of data!\n `year` must be an integer > 0"
               , fixed = TRUE)
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = factor("a"))
               , "Wrong type of data!\n `year` must be an integer > 0"
               , fixed = TRUE)
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = factor(1))
               , "Wrong type of data!\n `year` must be an integer > 0"
               , fixed = TRUE)
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = NULL)
               , "Wrong type of data!\n `year` must be an integer > 0"
               , fixed = TRUE)
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = NA)
               , "Wrong type of data!\n `year` must be an integer > 0"
               , fixed = TRUE)
  
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.relativeAbund", {

  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_allStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")

  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong type of data!\n `flag` (NB_FG) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("DO_LIGHT_COMPETITION 1\nDO_SOIL_COMPETITION 1\nNB_FG \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("DO_LIGHT_COMPETITION 1\nDO_SOIL_COMPETITION 1\nNB_FG 1\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--PFG_LIFE_HISTORY_PARAMS--\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt is different from the number of PFG files contained in FATE_simulation/DATA/PFGS/SUCC/"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--PFG_LIFE_HISTORY_PARAMS--\nFATE_simulation/PFG1.txt\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--PFG_LIFE_HISTORY_PARAMS--\nFATE_simulation/PFG1.txt\n--MASK--\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong name file given!\n `--END_OF_FILE--` does not exist"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--PFG_LIFE_HISTORY_PARAMS--\nFATE_simulation/PFG1.txt\n--MASK--\nFATE_simulation/Mask.asc\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/Mask.asc")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/ does not contain adequate files"
               , fixed = TRUE)
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_10_Hop_STRATA_all.tif")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Missing data!\n The names of PFG extracted from files within FATE_simulation/DATA/PFGS/SUCC/"
               , fixed = TRUE)
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_10_PFG1_STRATA_all.tif")

})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_validationStatistics", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10)
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame"
               , fixed = TRUE)
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.mat.PFG.obs = data.frame(PFG = "A", X = 1, Y = 2, obs = 1))
               , "Missing data!\n The names of PFG within `mat.PFG.obs` is different from the names of PFG contained from FATE_simulation/DATA/PFGS/SUCC/"
               , fixed = TRUE)
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.mat.PFG.obs = data.frame(PFG = "PFG1", X = 1, Y = 2, obs = 1))
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/ does not contain adequate files"
               , fixed = TRUE)
  # cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      # , file = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/Abund_YEAR_10_PFG1_STRATA_1.tif")
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_mapPFGvsHS", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE)
               , "Wrong type of data!\n `flag` (PFG_HAB_MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--PFG_LIFE_HISTORY_PARAMS--\nFATE_simulation/PFG1.txt\n--MASK--\nFATE_simulation/Mask.asc\n--PFG_HAB_MASK--\nFATE_simulation/Habsuit.asc\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/Habsuit.asc")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE)
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/BIN_perPFG_allStrata/ does not contain adequate files"
               , fixed = TRUE)
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/BIN_perPFG_allStrata/Binary_YEAR_10_PFG1_STRATA_all.tif")
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_mapPFGcover", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE)
               , "task 1 failed"
               , fixed = TRUE)
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_mapPFGrichness", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE)
               , "Wrong type of data!\n `mat.PFG.succ` must be a data.frame"
               , fixed = TRUE)
})

## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_mapPFGlight", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE
                                  , opt.doFunc.mapPFGrichness = FALSE)
               , "Wrong type of data!\n `mat.PFG.succ` must be a data.frame"
               , fixed = TRUE)
  # expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
  #                                 , file.simulParam = "ParamSimul.txt"
  #                                 , year = 10
  #                                 , opt.doFunc.validation = FALSE
  #                                 , opt.doFunc.mapPFGvsHS = FALSE
  #                                 , opt.doFunc.mapPFGcover = FALSE
  #                                 , opt.doFunc.mapPFGrichness = FALSE
  #                                 , opt.mat.PFG.succ = data.frame(PFG = "PFG1", light = 1))
  #              , "objet 'opt.mat.light.obs' introuvable"
  #              , fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_mapPFGsoil", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE
                                  , opt.doFunc.mapPFGrichness = FALSE
                                  , opt.doFunc.mapPFGlight = FALSE)
               , "Wrong type of data!\n `mat.PFG.succ` must be a data.frame"
               , fixed = TRUE)
  # expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
  #                                 , file.simulParam = "ParamSimul.txt"
  #                                 , year = 10
  #                                 , opt.doFunc.validation = FALSE
  #                                 , opt.doFunc.mapPFGvsHS = FALSE
  #                                 , opt.doFunc.mapPFGcover = FALSE
  #                                 , opt.doFunc.mapPFGrichness = FALSE
  #                                 , opt.doFunc.mapPFGlight = FALSE
  #                                 , opt.mat.PFG.succ = data.frame(PFG = "PFG1", soil_contrib = 1))
  #              , "objet 'opt.mat.soil.obs' introuvable"
  #              , fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_evolutionCoverage", {
  # expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
  #                                 , file.simulParam = "ParamSimul.txt"
  #                                 , year = 10
  #                                 , opt.doFunc.validation = FALSE
  #                                 , opt.doFunc.mapPFGvsHS = FALSE
  #                                 , opt.doFunc.mapPFGcover = FALSE
  #                                 , opt.doFunc.mapPFGrichness = FALSE
  #                                 , opt.doFunc.mapPFGlight = FALSE
  #                                 , opt.doFunc.mapPFGsoil = FALSE
  #                                 , no.years = 1)
  #              , "toutou"
  #              , fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_evolutionLight_pixels", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE
                                  , opt.doFunc.mapPFGrichness = FALSE
                                  , opt.doFunc.mapPFGlight = FALSE
                                  , opt.doFunc.mapPFGsoil = FALSE
                                  , no.years = 1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/LIGHT/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/LIGHT/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE
                                  , opt.doFunc.mapPFGrichness = FALSE
                                  , opt.doFunc.mapPFGlight = FALSE
                                  , opt.doFunc.mapPFGsoil = FALSE
                                  , no.years = 1)
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/LIGHT/ does not contain adequate files"
               , fixed = TRUE)
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/LIGHT/Light_Resources_YEAR_10_STRATA_1.tif")
})


## INPUTS
test_that("POST_FATE.graphics gives error with wrong data : folders / files : POST_FATE.graphic_evolutionSoil_pixels", {
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE
                                  , opt.doFunc.mapPFGrichness = FALSE
                                  , opt.doFunc.mapPFGlight = FALSE
                                  , opt.doFunc.mapPFGsoil = FALSE
                                  , no.years = 1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/SOIL/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/SOIL/")
  expect_error(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                  , file.simulParam = "ParamSimul.txt"
                                  , year = 10
                                  , opt.doFunc.validation = FALSE
                                  , opt.doFunc.mapPFGvsHS = FALSE
                                  , opt.doFunc.mapPFGcover = FALSE
                                  , opt.doFunc.mapPFGrichness = FALSE
                                  , opt.doFunc.mapPFGlight = FALSE
                                  , opt.doFunc.mapPFGsoil = FALSE
                                  , no.years = 1)
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/SOIL/ does not contain adequate files"
               , fixed = TRUE)
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/SOIL/Soil_Resources_YEAR_10.tif")
})


## OUTPUTS
test_that("POST_FATE.graphics gives right output", {
  expect_output(str(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10
                                       , opt.doFunc.validation = FALSE
                                       , opt.doFunc.mapPFGvsHS = FALSE
                                       , opt.doFunc.mapPFGcover = FALSE
                                       , opt.doFunc.mapPFGrichness = FALSE
                                       , opt.doFunc.mapPFGlight = FALSE
                                       , opt.doFunc.mapPFGsoil = FALSE
                                       , opt.doFunc.evolutionCoverage = FALSE
                                       , opt.doFunc.evolutionAbund_pixels = FALSE
                                       , opt.doFunc.evolutionLight_pixels = FALSE
                                       , opt.doFunc.evolutionSoil_pixels = FALSE)), "List")
  expect_output(str(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                       , year = 10
                                       , opt.doFunc.validation = FALSE
                                       , opt.doFunc.mapPFGvsHS = FALSE
                                       , opt.doFunc.mapPFGcover = FALSE
                                       , opt.doFunc.mapPFGrichness = FALSE
                                       , opt.doFunc.mapPFGlight = FALSE
                                       , opt.doFunc.mapPFGsoil = FALSE
                                       , opt.doFunc.evolutionCoverage = FALSE
                                       , opt.doFunc.evolutionAbund_pixels = FALSE
                                       , opt.doFunc.evolutionLight_pixels = FALSE
                                       , opt.doFunc.evolutionSoil_pixels = FALSE)), "List")
  expect_equal(length(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                         , file.simulParam = "ParamSimul.txt"
                                         , year = 10
                                         , opt.doFunc.validation = FALSE
                                         , opt.doFunc.mapPFGvsHS = FALSE
                                         , opt.doFunc.mapPFGcover = FALSE
                                         , opt.doFunc.mapPFGrichness = FALSE
                                         , opt.doFunc.mapPFGlight = FALSE
                                         , opt.doFunc.mapPFGsoil = FALSE
                                         , opt.doFunc.evolutionCoverage = FALSE
                                         , opt.doFunc.evolutionAbund_pixels = FALSE
                                         , opt.doFunc.evolutionLight_pixels = FALSE
                                         , opt.doFunc.evolutionSoil_pixels = FALSE)), 1)
  expect_equal(length(POST_FATE.graphics(name.simulation = "FATE_simulation"
                                         , file.simulParam = "ParamSimul.txt"
                                         , year = 10
                                         , opt.doFunc.validation = FALSE
                                         , opt.doFunc.mapPFGvsHS = TRUE
                                         , opt.doFunc.mapPFGcover = FALSE
                                         , opt.doFunc.mapPFGrichness = FALSE
                                         , opt.doFunc.mapPFGlight = FALSE
                                         , opt.doFunc.mapPFGsoil = FALSE
                                         , opt.doFunc.evolutionCoverage = FALSE
                                         , opt.doFunc.evolutionAbund_pixels = FALSE
                                         , opt.doFunc.evolutionLight_pixels = FALSE
                                         , opt.doFunc.evolutionSoil_pixels = FALSE)), 1)
})

