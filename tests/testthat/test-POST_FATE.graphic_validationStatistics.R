library(RFate)
context("POST_FATE.graphic_validationStatistics() function")


## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with missing data", {
  expect_error(POST_FATE.graphic_validationStatistics()
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(NA)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(NULL)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_validationStatistics(1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics("a")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(factor("A"))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(data.frame(1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : file.simulParam", {
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = NULL)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "toto")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `year` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : year", {
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = "a")
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = factor("a"))
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = factor(1))
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = NULL)
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = NA)
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10)
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : mat.PFG.obs", {
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = NA)
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = NULL)
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = "")
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = 1)
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = factor(1))
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = matrix(1))
               , "Wrong type of data!\n `mat.PFG.obs` must be a data.frame")
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame())
               , "`mat.PFG.obs` does not have the appropriate number of rows (>0) or columns (PFG, X, Y, obs)", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(1))
               , "`mat.PFG.obs` does not have the appropriate number of rows (>0) or columns (PFG, X, Y, obs)", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.obs` must be `PFG`, `X`, `Y` and `obs`")
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 2, Y = 3, obs = 4))
               , "`mat.PFG.obs$PFG` must contain a character value of length > 0", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = NA, X = 2, Y = 3, obs = 4))
               , "`mat.PFG.obs$PFG` must contain a character value of length > 0", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = factor("a"), X = 2, Y = 3, obs = 4))
               , "`mat.PFG.obs$PFG` must contain a character value of length > 0", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "", X = 2, Y = 3, obs = 4))
               , "`mat.PFG.obs$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = "2", Y = 3, obs = 4, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = factor(2), Y = 3, obs = 4, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = NA, Y = 3, obs = 4, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = "3", obs = 4, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = factor(3), obs = 4, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = NA, obs = 4, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = "4", stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = factor(4), stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = NA, stringsAsFactors = F))
               , "Columns `X`, `Y` and `obs` of `mat.PFG.obs` must contain numeric values", fixed = T)
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 4, stringsAsFactors = F))
               , "Column `obs` of `mat.PFG.obs` must contain either 0 or 1", fixed = T)
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : opt.ras_habitat", {
  if (file.exists("aaa")) file.remove("aaa")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                   , file.simulParam = "ParamSimul.txt"
                                                   , year = 10
                                                   , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F)
                                                   , opt.ras_habitat = "aaa")
               , "Wrong name file given!\n `aaa` does not exist")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : files", {
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)  
  cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("SAVE_DIR\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--SAVE_DIR--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (SAVE_DIR) does not contain any value"
               , fixed = TRUE)
  cat("--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_allStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")
  
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  

  if (file.exists("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")) file.remove("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  cat("GLOBAL_PARAMS\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--GLOBAL_PARAMS--\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong name file given!\n `FATE_simulation/GlobalParam.txt` does not exist"
               , fixed = TRUE)
  
  file.create("FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("HOP \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (NB_FG) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("NB_FG \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("NB_FG a\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("NB_FG 3\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--PFG_LIFE_HISTORY_PARAMS--\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt is different from the number of PFG files contained in FATE_simulation/DATA/PFGS/SUCC/"
               , fixed = TRUE)
  cat("NB_FG 1\n", file = "FATE_simulation/GlobalParam.txt")

  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--MASK--\nFATE_simulation/Mask.asc\n--PFG_LIFE_HISTORY_PARAMS--\nFATE_simulation/DATA/PFGS/SUCC/SUCC_A.txt\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Wrong name file given!\n `FATE_simulation/Mask.asc` does not exist"
               , fixed = TRUE)
  
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/Mask.asc")

  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/ABUND_REL_perPFG_allStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/Hello/ABUND_REL_perPFG_allStrata/Abund_relative_YEAR_10_PFG1_STRATA_all.tif")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , year = 10
                                                      , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F))
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/Abund_YEAR_10_PFG1_STRATA_1.tif")
})

## OUTPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : outputs", {
  expect_output(str(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                           , file.simulParam = "ParamSimul.txt"
                                                           , year = 10
                                                           , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F)))
                , "List")
  expect_output(str(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                           , year = 10
                                                           , mat.PFG.obs = data.frame(PFG = "A", X = 2, Y = 3, obs = 0, stringsAsFactors = F)))
                , "List")
})


