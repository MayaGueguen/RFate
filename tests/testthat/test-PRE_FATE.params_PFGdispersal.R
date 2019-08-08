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
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
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
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation", mat.PFG.disp = data.frame(1, 2, 3, 4))
               , "Column names of `mat.PFG.disp` must be `PFG`, `d50`, `d99` and `ldd`")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = c(2,2), d99 = 3, ldd = 4))
               , "Column `PFG` of `mat.PFG.disp` must contain different values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = "", d50 = NA, d99 = 3, ldd = 4))
               , "`mat.PFG.disp$PFG` must contain a character value of length > 0", fixed = T)
  

  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = c(2,NA), d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must not contain NA values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = NA, d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = "a", d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = factor(1), d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = factor("a"), d99 = 3, ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = 2, d99 = c(3,NA), ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must not contain NA values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = NA, ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = "a", ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor(1), ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = factor("a"), ldd = 4))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = c(1,2), d50 = 2, d99 = 3, ldd = c(4,NA)))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must not contain NA values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = NA))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = "a"))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor(1)))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
  expect_error(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = factor("a")))
               , "Wrong type of data!\n Columns `d50`, `d99` and `ldd` of `mat.PFG.disp` must contain numeric values")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGdispersal gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                            , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4))
               , "The parameter file FATE_simulation/DATA/PFGS/DISP/DISP_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4))
                 , "already exists. It will be replaced.")
  
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                                              , mat.PFG.disp = data.frame(PFG = 1, d50 = 2, d99 = 3, ldd = 4)
                                              , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/DISP/scen1/DISP_1.txt has been successfully created !")
})
