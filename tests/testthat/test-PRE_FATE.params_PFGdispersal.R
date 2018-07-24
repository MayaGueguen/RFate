library(RFate)
context("PRE_FATE.params_PFGdispersal() function")

## INPUTS
test_that("PRE_FATE.params_PFGdispersal gives error with missing data", {
  expect_error(PRE_FATE.params_PFGdispersal()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGdispersal gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGdispersal(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  expect_error(PRE_FATE.params_PFGdispersal(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGdispersal gives error with wrong data : mat.PFG.disp", {
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation")
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = NA)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = NULL)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = "")
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = 1)
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = factor(1))
               , "`mat.PFG.disp` must be a data.frame")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = matrix(1))
               , "`mat.PFG.disp` must be a data.frame")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame())
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)", fixed = T)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1))
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, d50, d99, ldd)", fixed = T)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.disp` must be `PFG`, `d50`, `d99` and `ldd`")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = c(2,2), d99 = 3, ldd = 4))
               , "Column `PFG` of `mat.PFG.disp` must contain different values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = NA, d99 = 3, ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = "a", d99 = 3, ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = factor(1), d99 = 3, ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = factor("a"), d99 = 3, ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = NA, ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = "a", ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor(1), ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor("a"), ldd = 4))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = NA))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = "a"))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor(1)))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor("a")))
               , "Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGdispersal gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4))
               , "The parameter file FATE_simulation/DATA/PFGS/DISP/DISP_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4))
                 , "already exists. It will be replaced.")
})
