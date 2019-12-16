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
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
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
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_...)"
               , fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_...)"
               , fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (NAME, TYPE, MATURITY, LONGEVITY, STRATA, CHANG_STR_AGES_to_str_...)"
               , fixed = T)
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = 2, MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, HOP = 1))
               , "Column names of `mat.PFG.succ` must be `NAME`, `TYPE`, `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...`")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = c(2,2), MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Column `NAME` of `mat.PFG.succ` must contain different values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = NA, TYPE = 2, MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Column `NAME` of `mat.PFG.succ` must contain different values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = c(1,NA), TYPE = c(2,2), MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Column `NAME` of `mat.PFG.succ` must contain different values")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = NA, MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "`mat.PFG.succ$TYPE` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = 2, MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "`mat.PFG.succ$TYPE` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "", MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "`mat.PFG.succ$TYPE` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = c(2,NA), MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "`mat.PFG.succ$TYPE` must be either `H`, `C` or `P`", fixed = T)
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = NA
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = "a"
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = factor(1)
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = "H", MATURITY = c(4,NA)
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = NA, STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = "a", STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = factor(1), STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = c(10,NA), STRATA = 1, CHANG_STR_AGES_to_str_1 = 1))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = NA))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = "a"))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = 1, TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = factor(1)))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.succ = data.frame(NAME = c(1,2), TYPE = "H", MATURITY = 4
                                                                          , LONGEVITY = 10, STRATA = 1, CHANG_STR_AGES_to_str_1 = c(1,NA)))
               , "Columns `MATURITY`, `LONGEVITY`, `STRATA` and `CHANG_STR_AGES_to_str_...` of `mat.PFG.succ` must not contain NA values")
})


## INPUTS
test_that("PRE_FATE.params_PFGdisturbance gives error with wrong data : mat.PFG.dist", {
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)))
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
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (name, responseStage, PFG, KilledIndiv, ResproutIndiv)", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = data.frame(1))
               , "`mat.PFG.dist` does not have the appropriate number of rows (>0) or columns (name, responseStage, PFG, KilledIndiv, ResproutIndiv)", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation", mat.PFG.dist = data.frame(1, 2, 3, 4, 5))
               , "Column names of `mat.PFG.dist` must be `name`, `responseStage`, `PFG`, `KilledIndiv` and `ResproutIndiv`")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = "", responseStage = -1, PFG = "", KilledIndiv = 0, ResproutIndiv = 0))
               , "Wrong type of data!\n `mat.PFG.dist$name` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = "A", responseStage = -1, PFG = "", KilledIndiv = 0, ResproutIndiv = 0))
               , "Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1.5, PFG = "", KilledIndiv = 0, ResproutIndiv = 0))
               , "Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = "a", PFG = "", KilledIndiv = 0, ResproutIndiv = 0))
               , "Column `responseStage` of `mat.PFG.dist` must contain values between 1 and 4")
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "", KilledIndiv = 0, ResproutIndiv = 0))
               , "Wrong type of data!\n `mat.PFG.dist$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "a", KilledIndiv = 0, ResproutIndiv = 0))
               , "Wrong type of data!\n `mat.PFG.dist$PFG` must be either `H`, `C`, `P`, `PFG1`, `PFG2`, `PFG3`, `PFG4`, `PFG5` or `PFG6`", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "h", KilledIndiv = 0, ResproutIndiv = 0))
               , "Wrong type of data!\n `mat.PFG.dist$PFG` must be either `H`, `C`, `P`, `PFG1`, `PFG2`, `PFG3`, `PFG4`, `PFG5` or `PFG6`", fixed = T)
  
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "H", KilledIndiv = "", ResproutIndiv = 0))
               , "Column `KilledIndiv` of `mat.PFG.dist` must contain values between 0 and 10", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "H", KilledIndiv = -1, ResproutIndiv = 0))
               , "Column `KilledIndiv` of `mat.PFG.dist` must contain values between 0 and 10", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "H", KilledIndiv = 1.5, ResproutIndiv = 0))
               , "Column `KilledIndiv` of `mat.PFG.dist` must contain values between 0 and 10", fixed = T)
  
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "H", KilledIndiv = 0, ResproutIndiv = ""))
               , "Column `ResproutIndiv` of `mat.PFG.dist` must contain values between 0 and 10", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "H", KilledIndiv = 0, ResproutIndiv = -1))
               , "Column `ResproutIndiv` of `mat.PFG.dist` must contain values between 0 and 10", fixed = T)
  expect_error(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                              , mat.PFG.dist = data.frame(name = 1, responseStage = 1, PFG = "H", KilledIndiv = 0, ResproutIndiv = 1.5))
               , "Column `ResproutIndiv` of `mat.PFG.dist` must contain values between 0 and 10", fixed = T)
})


## OUTPUTS
test_that("PRE_FATE.params_PFGdisturbance gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)))
  
  expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                , mat.PFG.dist = data.frame(name = "DIST1"
                                                                            , responseStage = 1
                                                                            , PFG = "C"
                                                                            , KilledIndiv = 0
                                                                            , ResproutIndiv = 0))
                 , "The parameter file FATE_simulation/DATA/PFGS/DIST/DIST_PFG1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                , mat.PFG.dist = data.frame(name = "DIST1"
                                                                            , responseStage = 1
                                                                            , PFG = "PFG1"
                                                                            , KilledIndiv = 0
                                                                            , ResproutIndiv = 0))
                 , "already exists. It will be replaced.")
  
  expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                , mat.PFG.dist = data.frame(name = "DIST1"
                                                                            , responseStage = 1
                                                                            , PFG = "H"
                                                                            , KilledIndiv = 0
                                                                            , ResproutIndiv = 0)
                                                , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                , mat.PFG.dist = data.frame(name = "DIST1"
                                                                            , responseStage = 1
                                                                            , PFG = "P"
                                                                            , KilledIndiv = 0
                                                                            , ResproutIndiv = 0)
                                                , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                                , mat.PFG.dist = data.frame(name = "DIST1"
                                                                            , responseStage = 1
                                                                            , PFG = "PFG6"
                                                                            , KilledIndiv = 0
                                                                            , ResproutIndiv = 0)
                                                , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/DIST/scen1/DIST_PFG6.txt has been successfully created !")
})
