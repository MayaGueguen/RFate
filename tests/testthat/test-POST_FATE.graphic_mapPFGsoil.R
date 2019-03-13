library(RFate)
context("POST_FATE.graphic_mapPFGsoil() function")


## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with missing data", {
  expect_error(POST_FATE.graphic_mapPFGsoil()
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGsoil(NA)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGsoil(NULL)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_mapPFGsoil(1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGsoil("a")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGsoil(factor("A"))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_mapPFGsoil(data.frame(1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  system("mkdir FATE_simulation/")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  system("mkdir FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  system("mkdir FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
  system("mkdir FATE_simulation/DATA/")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : file.simulParam", {
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = NULL)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "toto")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `year` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : year", {
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = "a")
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = factor("a"))
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = factor(1))
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = NULL)
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = NA)
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10)
               , "Wrong type of data!\n `mat.PFG.succ` must be a data.frame")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : mat.PFG.succ", {
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = NA)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = NULL)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = "")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = 1)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = factor(1))
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = matrix(1))
               , "`mat.PFG.succ` must be a data.frame")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame())
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, soil_contrib)", fixed = T)
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, soil_contrib)", fixed = T)
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(1,2))
                            , "Column names of `mat.PFG.succ` must be `PFG` and `soil_contrib`")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = c(1,1)))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = NA, soil_contrib = 1))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = c(1,NA), soil_contrib = 1))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = "", soil_contrib = 1))
               , "`mat.PFG.succ$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = NA))
               , "Columns `soil_contrib` of `mat.PFG.succ` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = "a"))
               , "Columns `soil_contrib` of `mat.PFG.succ` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = factor(1)))
               , "Columns `soil_contrib` of `mat.PFG.succ` must contain numeric values")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), soil_contrib = c(2, NA)))
               , "Columns `soil_contrib` of `mat.PFG.succ` must not contain NA values")
})


## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : strata_min", {
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = "a")
               , "Wrong type of data!\n `strata_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = factor("a"))
               , "Wrong type of data!\n `strata_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = factor(1))
               , "Wrong type of data!\n `strata_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = NULL)
               , "Wrong type of data!\n `strata_min` must be an integer > 0")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = NA)
               , "Wrong type of data!\n `strata_min` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : opt.mat.soil.obs", {
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = "a")
               , "Wrong type of data!\n `opt.mat.soil.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = factor("a"))
               , "Wrong type of data!\n `opt.mat.soil.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = factor(1))
               , "Wrong type of data!\n `opt.mat.soil.obs` must be a data.frame")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(1))
               , "Wrong dimension(s) of data!\n `opt.mat.soil.obs` does not have the appropriate number of rows (>0) or columns (X, Y, obs)"
               , fixed = TRUE)
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(1, 2, 3))
               , "Wrong type of data!\n Column names of `opt.mat.soil.obs` must be `X`, `Y` and `obs`")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(X = "a", Y = 2, obs = 3))
               , "Wrong type of data!\n Columns `X`, `Y` and `obs` of `opt.mat.soil.obs` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(X = factor(1), Y = 2, obs = 3))
               , "Wrong type of data!\n Columns `X`, `Y` and `obs` of `opt.mat.soil.obs` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(X = 1, Y = "a", obs = 3))
               , "Wrong type of data!\n Columns `X`, `Y` and `obs` of `opt.mat.soil.obs` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(X = 1, Y = factor(1), obs = 3))
               , "Wrong type of data!\n Columns `X`, `Y` and `obs` of `opt.mat.soil.obs` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(X = 1, Y = 2, obs = "a"))
               , "Wrong type of data!\n Columns `X`, `Y` and `obs` of `opt.mat.soil.obs` must contain numeric values")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.mat.soil.obs = data.frame(X = 1, Y = 2, obs = factor(1)))
               , "Wrong type of data!\n Columns `X`, `Y` and `obs` of `opt.mat.soil.obs` must contain numeric values")
})


## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : opt.ras.soil.obs", {
  if (file.exists("aaa")) file.remove("aaa")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1)
                                             , opt.ras.soil.obs = "aaa")
               , "Wrong name file given!\n `aaa` does not exist")
  file.create("aaa")
  # expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
  #                                            , file.simulParam = "ParamSimul.txt"
  #                                            , year = 10
  #                                            , strata_min = 2
  #                                            , opt.ras.soil.obs = "aaa")
  #              , "Cannot create a RasterLayer object from this file."
  #              , fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : files", {
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)  
  cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("SAVE_DIR\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--SAVE_DIR--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (SAVE_DIR) does not contain any value"
               , fixed = TRUE)
  cat("--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
               , fixed = TRUE)
  system("mkdir FATE_simulation/RESULTS/Hello/")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/BIN_perPFG_allStrata/ folder"
               , fixed = TRUE)
  system("mkdir FATE_simulation/RESULTS/Hello/BIN_perPFG_allStrata/")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
               , fixed = TRUE)
  system("mkdir FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")
  
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  
  if (file.exists("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")) system("rm FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  cat("GLOBAL_PARAMS\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--GLOBAL_PARAMS--\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong name file given!\n `FATE_simulation/GlobalParam.txt` does not exist"
               , fixed = TRUE)
  
  file.create("FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("HOP \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (NB_FG) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("NB_FG \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("NB_FG a\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("NB_FG 3\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--PFG_LIFE_HISTORY_PARAMS--\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt is different from the number of PFG files contained in FATE_simulation/DATA/PFGS/SUCC/"
               , fixed = TRUE)
  cat("NB_FG 1\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--MASK--\nFATE_simulation/Mask.asc\n--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong name file given!\n `FATE_simulation/Mask.asc` does not exist"
               , fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphic_mapPFGsoil gives error with wrong data : rasters", {
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/Mask.asc")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/ does not contain adequate files"
               , fixed = TRUE)
  
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/Abund_YEAR_10_Hop_STRATA_1.tif")
  expect_error(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
                                             , file.simulParam = "ParamSimul.txt"
                                             , year = 10
                                             , strata_min = 2
                                             , mat.PFG.succ = data.frame(PFG = 1, soil_contrib = 1))
               , "Wrong data given!\n `strata_min` is superior to maximum strata found (1)"
               , fixed = TRUE)
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/Abund_YEAR_10_Hop_STRATA_2.tif")
  # expect_message(POST_FATE.graphic_mapPFGsoil(name.simulation = "FATE_simulation"
  #                                            , file.simulParam = "ParamSimul.txt"
  #                                            , year = 10
  #                                            , strata_min = 2)
  #              , "has been successfully created"
  #              , fixed = TRUE)
})

