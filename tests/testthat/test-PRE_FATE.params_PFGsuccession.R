library(RFate)
context("PRE_FATE.params_PFGsuccession() function")

## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with missing data", {
  expect_error(PRE_FATE.params_PFGsuccession()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGsuccession(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  expect_error(PRE_FATE.params_PFGsuccession(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGsuccession gives error with wrong data : mat.PFG.succ", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = NA)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = NULL)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = "")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = 1)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = factor(1))
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = matrix(1))
               , "`mat.PFG.succ` must be a data.frame")
  
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = data.frame())
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity)", fixed = T)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity)", fixed = T)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation", mat.PFG.succ = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity` and `longevity`")
  
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                            , mat.PFG.succ = data.frame(PFG = 1, type = c(2,2), height = 3, maturity = 4
                                                                        , longevity = 10))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = NA, type = 2, height = 3, maturity = 4
                                                                         , longevity = 10))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,NA), type = c(2,2), height = 3, maturity = 4
                                                                         , longevity = 10))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = "", type = NA, height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = NA, height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = 2, height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "", height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = c(2,NA), height = 3, maturity = 4
                                                                         , longevity = 10))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = NA, maturity = 4
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = "a", maturity = 4
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = factor(1), maturity = 4
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = c(3,NA), maturity = 4
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must not contain NA values")

  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = NA
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = "a"
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = factor(1)
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = 3, maturity = c(4,NA)
                                                                         , longevity = 10))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                         , longevity = NA))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                         , longevity = "a"))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                         , longevity = factor(1)))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                             , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = 3, maturity = 4
                                                                         , longevity = c(10,NA)))
               , "Columns `height`, `maturity` and `longevity` of `mat.PFG.succ` must not contain NA values")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsuccession gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)))
               , "The parameter file FATE_simulation/DATA/PFGS/SUCC/SUCC_PFG1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                               , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                           , type = c("C", "C", "H", "H", "P", "P")
                                                                           , height = c(10, 250, 36, 68, 1250, 550)
                                                                           , maturity = c(5, 5, 3, 3, 8, 9)
                                                                           , longevity = c(12, 200, 25, 4, 110, 70)))
                 , "already exists. It will be replaced.")
})
