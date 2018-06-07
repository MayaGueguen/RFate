library(RFate)
context("PRE_FATE.params_PFGdisturbance() function")

## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with missing data", {
  expect_error(PRE_FATE.params_PFGdisturbance()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGdisturbance(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  expect_error(PRE_FATE.params_PFGdisturbance(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with wrong data : mat.PFG.succ", {
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation")
               , "`mat.PFG.succ` must be an existing `.csv` or `.txt` filename with a header and space separator")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.succ = NA)
               , "`mat.PFG.succ` must be either :\n ==> an existing `.csv` or `.txt`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.succ = NULL)
               , "`mat.PFG.succ` must be either :\n ==> an existing `.csv` or `.txt`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.succ = "")
               , "`mat.PFG.succ` must be an existing `.csv` or `.txt` filename with a header and space separator")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.succ = 1)
               , "`mat.PFG.succ` must be either :\n ==> an existing `.csv` or `.txt`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.succ = factor(1))
               , "`mat.PFG.succ` must be either :\n ==> an existing `.csv` or `.txt`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.succ = matrix(1))
               , "`mat.PFG.succ` must be either :\n ==> an existing `.csv` or `.txt`")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame())
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (at least NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_3)"
               , fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (at least NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_3)"
               , fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (at least NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_3)"
               , fixed = T)
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = 2, MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, HOP = 1))
               , "Column names of `mat.PFG.succ` must contain `NAME`, `TYPE`, `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3`")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = c(2,2), MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `NAME` of `mat.PFG.succ` must contain different values and no NA values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = NA, TYPE = 2, MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `NAME` of `mat.PFG.succ` must contain different values and no NA values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = c(1,NA), TYPE = c(2,2), MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `NAME` of `mat.PFG.succ` must contain different values and no NA values")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = NA, MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `TYPE` of `mat.PFG.succ` must contain values such as `H`, `C` or `P`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = 2, MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `TYPE` of `mat.PFG.succ` must contain values such as `H`, `C` or `P`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "", MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `TYPE` of `mat.PFG.succ` must contain values such as `H`, `C` or `P`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = c(2,NA), MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Column `TYPE` of `mat.PFG.succ` must contain values such as `H`, `C` or `P`")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = NA
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = "a"
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = factor(1)
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = "H", MATURITY = c(4,NA)
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = NA, STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = "a", STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = factor(1), STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = c(10,NA), STRATA = 1, CHANG_STR_AGES_to_str_3 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = NA))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = "a"))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = factor(1)))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = "H", MATURITY = 4
                                                                         , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_3 = c(1,NA)))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_3` of `mat.PFG.succ` must not contain NA values")
  

  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)
                                                            , dispersal = 1
                                                            , light = c(4, 6, 3, 6, 5, 5)))
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation")
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = NA)
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = NULL)
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = "")
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = 1)
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = factor(1))
               , "`mat.PFG.dist` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = matrix(1))
               , "`mat.PFG.dist` must be a data.frame")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = data.frame())
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (at least `name` and `responseStage`)", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = data.frame(1))
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (at least `name` and `responseStage`)", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = data.frame(1,2,3,4,5,6,7))
               , "Column names of `mat.PFG.dist` must contain `name` and `responseStage`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, stage = 1))
               , "Column names of `mat.PFG.dist` must contain `name` and `responseStage`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = -1))
               , "Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1.5))
               , "Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = "a"))
               , "Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `KilledIndiv_H`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1
                                                                          , KilledIndiv_H = 0
                                                                          , KilledIndiv_H = 0
                                                                          , KilledIndiv_H = 0))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `KilledIndiv_H`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1
                                                                          , KilledIndiv_H = 0
                                                                          , KilledIndiv_C = 0
                                                                          , KilledIndiv_P = 0))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `ResproutIndiv_H`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1
                                                                          , KilledIndiv_H = 0
                                                                          , KilledIndiv_C = 0
                                                                          , KilledIndiv_P = 0
                                                                          , ResproutIndiv_H = 0
                                                                          , ResproutIndiv_H = 0
                                                                          , ResproutIndiv_H = 0))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `ResproutIndiv_H`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1
                                                                          , KilledIndiv_1 = 0))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `KilledIndiv_H`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1
                                                                          , KilledIndiv_1 = 0
                                                                          , ResproutIndiv_1 = 0))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `KilledIndiv_H`")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1
                                                                          , KilledIndiv_H = 0
                                                                          , KilledIndiv_C = 0
                                                                          , KilledIndiv_P = 0
                                                                          , ResproutIndiv_1 = 0))
               , "Column names of `mat.PFG.dist` must contain either :\n ==> `ResproutIndiv_H`")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGdisturbance gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)
                                                            , dispersal = 1
                                                            , light = c(4, 6, 3, 6, 5, 5)))
  
  expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                               , mat.PFG.dist = data.frame(name = "DIST1", responseStage = 1
                                                                           , KilledIndiv_H = 0
                                                                           , KilledIndiv_C = 0
                                                                           , KilledIndiv_P = 0
                                                                           , ResproutIndiv_H = 0
                                                                           , ResproutIndiv_C = 0
                                                                           , ResproutIndiv_P = 0))
               , "The parameter file FATE_simulation/DATA/PFGS/DIST/DIST_PFG1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                               , mat.PFG.dist = data.frame(name = "DIST1", responseStage = 1
                                                                           , KilledIndiv_H = 0
                                                                           , KilledIndiv_C = 0
                                                                           , KilledIndiv_P = 0
                                                                           , ResproutIndiv_H = 0
                                                                           , ResproutIndiv_C = 0
                                                                           , ResproutIndiv_P = 0))
                 , "already exists. It will be replaced.")
})
