library(RFate)
context("PRE_FATE.params_PFGlight() function")

## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with missing data", {
  expect_error(PRE_FATE.params_PFGlight()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGlight(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  expect_error(PRE_FATE.params_PFGlight(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with wrong data : mat.PFG.succ", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = NA)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = NULL)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = "")
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = 1)
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = factor(1))
               , "`mat.PFG.succ` must be a data.frame")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = matrix(1))
               , "`mat.PFG.succ` must be a data.frame")
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = data.frame())
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity, light)", fixed = T)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = data.frame(1))
               , "`mat.PFG.succ` does not have the appropriate number of rows (>0) or columns (PFG, type, height, maturity, longevity, light)", fixed = T)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation", mat.PFG.succ = data.frame(1,2,3,4,5,6))
               , "Column names of `mat.PFG.succ` must be `PFG`, `type`, `height`, `maturity`, `longevity` and `light`")
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = c(2,2), height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = NA, type = 2, height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = c(1,NA), type = c(2,2), height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "Column `PFG` of `mat.PFG.succ` must contain different values")
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = "", type = NA, height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "`mat.PFG.succ$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = NA, height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = 2, height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "", height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = c(1,2), type = c(2,NA), height = 3, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "`mat.PFG.succ$type` must be either `H`, `C` or `P`", fixed = T)
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = NA, maturity = 4
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = "a", maturity = 4
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = factor(1), maturity = 4
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = c(3,NA), maturity = 4
                                                                    , longevity = 10,  light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = NA
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = "a"
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = factor(1)
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = 3, maturity = c(4,NA)
                                                                    , longevity = 10, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = NA, light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = "a", light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = factor(1), light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = 3, maturity = 4
                                                                    , longevity = c(10,NA), light = 2))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must not contain NA values")
  
  
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = NA))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = "a"))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = factor(1)))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must contain numeric values")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = -1))
               , "Column `light` of `mat.PFG.succ` must contain values between 0 and 10")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = 1.5))
               , "Column `light` of `mat.PFG.succ` must contain values between 0 and 10")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = 1, type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = 11))
               , "Column `light` of `mat.PFG.succ` must contain values between 0 and 10")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = c(1,2), type = "H", height = 3, maturity = 4
                                                                    , longevity = 10, light = c(2,NA)))
               , "Columns `height`, `maturity`, `longevity` and `light` of `mat.PFG.succ` must not contain NA values")
})


## INPUTS
test_that("PRE_FATE.params_PFGlight gives error with wrong data : strata.limits", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                    , type = c("C", "C", "H", "H", "P", "P")
                                                                    , height = c(10, 250, 36, 68, 1250, 550)
                                                                    , maturity = c(5, 5, 3, 3, 8, 9)
                                                                    , longevity = c(12, 200, 25, 4, 110, 70)
                                                                    , light = c(4, 6, 3, 6, 5, 5))
                                        , strata.limits = NA)
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                    , type = c("C", "C", "H", "H", "P", "P")
                                                                    , height = c(10, 250, 36, 68, 1250, 550)
                                                                    , maturity = c(5, 5, 3, 3, 8, 9)
                                                                    , longevity = c(12, 200, 25, 4, 110, 70)
                                                                    , light = c(4, 6, 3, 6, 5, 5))
                                        , strata.limits = "a")
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                    , type = c("C", "C", "H", "H", "P", "P")
                                                                    , height = c(10, 250, 36, 68, 1250, 550)
                                                                    , maturity = c(5, 5, 3, 3, 8, 9)
                                                                    , longevity = c(12, 200, 25, 4, 110, 70)
                                                                    , light = c(4, 6, 3, 6, 5, 5))
                                        , strata.limits = factor(1))
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
  expect_error(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                        , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                    , type = c("C", "C", "H", "H", "P", "P")
                                                                    , height = c(10, 250, 36, 68, 1250, 550)
                                                                    , maturity = c(5, 5, 3, 3, 8, 9)
                                                                    , longevity = c(12, 200, 25, 4, 110, 70)
                                                                    , light = c(4, 6, 3, 6, 5, 5))
                                        , strata.limits = -1)
               , "Wrong type of data!\n `strata.limits` must be an integer > 0")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGlight gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                      , type = c("C", "C", "H", "H", "P", "P")
                                                                      , height = c(10, 250, 36, 68, 1250, 550)
                                                                      , maturity = c(5, 5, 3, 3, 8, 9)
                                                                      , longevity = c(12, 200, 25, 4, 110, 70)
                                                                      , light = c(4, 6, 3, 6, 5, 5)))
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/LIGHT_PFG1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                      , type = c("C", "C", "H", "H", "P", "P")
                                                                      , height = c(10, 250, 36, 68, 1250, 550)
                                                                      , maturity = c(5, 5, 3, 3, 8, 9)
                                                                      , longevity = c(12, 200, 25, 4, 110, 70)
                                                                      , light = c(4, 6, 3, 6, 5, 5)))
                 , "already exists. It will be replaced.")
  
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                      , type = c("C", "C", "H", "H", "P", "P")
                                                                      , height = c(10, 250, 36, 68, 1250, 550)
                                                                      , maturity = c(5, 5, 3, 3, 8, 9)
                                                                      , longevity = c(12, 200, 25, 4, 110, 70)
                                                                      , light = c(4, 6, 3, 6, 5, 5))
                                          , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                      , type = c("C", "C", "H", "H", "P", "P")
                                                                      , height = c(10, 250, 36, 68, 1250, 550)
                                                                      , maturity = c(5, 5, 3, 3, 8, 9)
                                                                      , longevity = c(12, 200, 25, 4, 110, 70)
                                                                      , light = c(4, 6, 3, 6, 5, 5))
                                          , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                                          , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                                      , type = c("C", "C", "H", "H", "P", "P")
                                                                      , height = c(10, 250, 36, 68, 1250, 550)
                                                                      , maturity = c(5, 5, 3, 3, 8, 9)
                                                                      , longevity = c(12, 200, 25, 4, 110, 70)
                                                                      , light = c(4, 6, 3, 6, 5, 5))
                                          , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/LIGHT/scen1/LIGHT_PFG1.txt has been successfully created !")
})
