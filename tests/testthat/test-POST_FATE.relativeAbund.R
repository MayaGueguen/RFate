library(RFate)
library(raster)
library(rgdal)
context("POST_FATE.relativeAbund() function")


## INPUTS
test_that("POST_FATE.relativeAbund gives error with missing data", {
  expect_error(POST_FATE.relativeAbund()
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(NA)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(NULL)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.relativeAbund(1)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund("a")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(factor("A"))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.relativeAbund(data.frame(1))
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : file.simulParam", {
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = NULL)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "toto")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt")
               , "Wrong type of data!\n `year` must be an integer > 0")
})


## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : year", {  
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = "a")
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = factor("a"))
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = factor(1))
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = NULL)
               , "Wrong type of data!\n `year` must be an integer > 0")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = NA)
               , "Wrong type of data!\n `year` must be an integer > 0")
})


## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : files", {
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)  
  cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("SAVE_DIR\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--SAVE_DIR--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (SAVE_DIR) does not contain any value"
               , fixed = TRUE)
  cat("--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/")
  
  
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_allStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
               , fixed = TRUE)
  dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")
  
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  
  if (file.exists("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")) system("rm FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  cat("GLOBAL_PARAMS\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--GLOBAL_PARAMS--\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong name file given!\n `FATE_simulation/GlobalParam.txt` does not exist"
               , fixed = TRUE)
  
  file.create("FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("HOP \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (NB_FG) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
               , fixed = TRUE)
  cat("NB_FG \n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("NB_FG a\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
               , fixed = TRUE)
  cat("NB_FG 3\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  cat("--PFG_LIFE_HISTORY_PARAMS--\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) does not contain any value"
               , fixed = TRUE)
  cat("--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt is different from the number of PFG files contained in FATE_simulation/DATA/PFGS/SUCC/"
               , fixed = TRUE)
  cat("NB_FG 1\n", file = "FATE_simulation/GlobalParam.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
               , fixed = TRUE)
  
  cat("--MASK--\nFATE_simulation/Mask.asc\n--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
      , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Wrong name file given!\n `FATE_simulation/Mask.asc` does not exist"
               , fixed = TRUE)
})

## INPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : rasters", {
  cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
      , file = "FATE_simulation/Mask.asc")
  expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
                                       , file.simulParam = "ParamSimul.txt"
                                       , year = 10)
               , "Missing data!\n The folder FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/ does not contain adequate files"
               , fixed = TRUE)
  
  # cat("ncols 3\nnrows 3\nxllcorner 1\nyllcorner 1\ncellsize 30\nnodata_value -999\n0 0 1\n0 1 1\n1 1 1"
  #     , file = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_10_HOP_STRATA_all.tif")
  # expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
  #                                                      , file.simulParam = "ParamSimul.txt"
  #                                                      , year = 10)
  #              , "Missing data!\n The names of PFG extracted from files within FATE_simulation/DATA/PFGS/SUCC/"
  #              , fixed = TRUE)
  
  # if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  # dir.create("FATE_Bauges/")
  # dir.create("FATE_Bauges/PARAM_SIMUL/")
  # dir.create("FATE_Bauges/RESULTS/")
  # dir.create("FATE_Bauges/DATA/")
  # dir.create("FATE_Bauges/DATA/MASK/")
  # data("FATE_Bauges")
  # write.table(FATE_Bauges$param.simul, file = "FATE_Bauges/PARAM_SIMUL/paramSimul_Graz1_CA_rcp26_LIGHT.txt"
  #             , col.names = FALSE, row.names = FALSE, quote = FALSE)
  # write.table(FATE_Bauges$param.global, file = "FATE_Bauges/DATA/Global_parameters_FUTUR_LIGHT.txt"
  #             , col.names = FALSE, row.names = FALSE, quote = FALSE)
  # 
  # writeRaster(FATE_Bauges$mask, filename = "FATE_Bauges/DATA/MASK/maskDemo.tif")
  # dir.save = FATE_Bauges$param.simul$V1[grep("SAVE_DIR", FATE_Bauges$param.simul$V1) + 1]
  # system(paste0("mkdir ", dir.save))
  # system(paste0("mkdir ", dir.save, "ABUND_perPFG_allStrata/"))
  # system(paste0("mkdir ", dir.save, "ABUND_perPFG_perStrata/"))
  # writeRaster(FATE_Bauges$ABUND_perPFG_allStrata$Abund_YEAR_20
  #             , filename = paste0(dir.save
  #                                 , "ABUND_perPFG_allStrata/"
  #                                 , names(FATE_Bauges$ABUND_perPFG_allStrata$Abund_YEAR_20))
  #             , bylayer = TRUE)
  # 
  # expect_error(POST_FATE.relativeAbund(name.simulation = "FATE_Bauges"
  #                                      , file.simulParam = "paramSimul_Graz1_CA_rcp26_LIGHT.txt"
  #                                      , year = 10)
  #              , "Missing data!\n The folder FATE_Bauges/RESULTS/Graz1_CA_rcp26_LIGHT/ABUND_perPFG_allStrata/ does not contain adequate files"
  #              , fixed = TRUE)
  # 
  # # expect_error(POST_FATE.relativeAbund(name.simulation = "RFate/data_supplements/FATE_Bauges_oldPFG_newParam/"
  # #                                      , file.simulParam = "RFate/data_supplements/FATE_Bauges_oldPFG_newParam/PARAM_SIMUL/paramSimul_Graz1_CA_rcp26_LIGHT.txt"
  # #                                      , year = 10)
  # #              , "Missing data!\n The folder RFate/data_supplements/FATE_Bauges_oldPFG_newParam/RESULTS/Graz1_CA_rcp26_LIGHT/ABUND_perPFG_allStrata/ does not contain adequate files"
  # #              , fixed = TRUE)
  # # expect_error(POST_FATE.relativeAbund(name.simulation = "data_supplements/FATE_Bauges_oldPFG_newParam/"
  # #                                      , file.simulParam = "data_supplements/FATE_Bauges_oldPFG_newParam/PARAM_SIMUL/paramSimul_Graz1_CA_rcp26_LIGHT.txt"
  # #                                      , year = 10)
  # #              , "Missing data!\n The names of PFG extracted from files within FATE_simulation/DATA/PFGS/SUCC/"
  # #              , fixed = TRUE)
})

## OUTPUTS
test_that("POST_FATE.relativeAbund gives error with wrong data : outputs", {
  # expect_output_file(POST_FATE.relativeAbund(name.simulation = "FATE_Bauges"
  #                                            , file.simulParam = "paramSimul_Graz1_CA_rcp26_LIGHT.txt"
  #                                            , year = 20)
  #                    , "FATE_Bauges/RESULTS/Graz1_CA_rcp26_LIGHT/ABUND_REL_perPFG_allStrata/Abund_relative_YEAR_20_C1_STRATA_all.txt"
  #                    , fixed = TRUE)
})

