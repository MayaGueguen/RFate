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
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
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
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, MODE, d50, d99, ldd)", fixed = T)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1))
               , "`mat.PFG.disp` does not have the appropriate number of rows (>0) or columns (PFG, MODE, d50, d99, ldd)", fixed = T)
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.disp` must be `PFG`, `MODE`, `d50`, `d99` and `ldd`")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = c(2,2), d99 = 3, ldd = 4))
               , "Column `PFG` of `mat.PFG.disp` must contain different values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = "", MODE = NA, d50 = 2, d99 = 3, ldd = 4))
               , "`mat.PFG.disp$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = NA, d50 = 2, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = "a", d50 = 2, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = factor(1), d50 = 2, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = factor("a"), d50 = 2, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1.5, d50 = 2, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Column `MODE` of `mat.PFG.disp` must contain values between 1 and 3")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 5, d50 = 2, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Column `MODE` of `mat.PFG.disp` must contain values between 1 and 3")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = NA, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = "a", d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = factor(1), d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = factor("a"), d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = NA, ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = "a", ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = factor(1), ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = factor("a"), ldd = 4))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = 3, ldd = NA))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = 3, ldd = "a"))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = 3, ldd = factor(1)))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = 3, ldd = factor("a")))
               , "Wrong type of data!\n Columns `MODE`, `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGdispersal gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = 3, ldd = 4))
               , "The parameter file FATE_simulation/DATA/PFGS/DISP/DISP_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, MODE = 1, d50 = 2, d99 = 3, ldd = 4))
                 , "already exists. It will be replaced.")
})
