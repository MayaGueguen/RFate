library(RFate)
context("PRE_FATE.params_PFGsoil() function")

## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with missing data", {
  expect_error(PRE_FATE.params_PFGsoil()
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(NA)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_PFGsoil(1)
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil("a")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  expect_error(PRE_FATE.params_PFGsoil(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with wrong data : mat.PFG.soil", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation")
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = NA)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = NULL)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = "")
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = 1)
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = factor(1))
               , "`mat.PFG.soil` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = matrix(1))
               , "`mat.PFG.soil` must be a data.frame")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame())
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, soil_contrib, soil_tol_min, soil_tol_max)", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame(1))
               , "`mat.PFG.soil` does not have the appropriate number of rows (>0) or columns (PFG, type, soil_contrib, soil_tol_min, soil_tol_max)", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation", mat.PFG.soil = data.frame(1,2,3,4,5))
               , "Column names of `mat.PFG.soil` must be `PFG`, `type`, `soil_contrib`, `soil_tol_min` and `soil_tol_max`")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = c(2,2), soil_tol_max = 3))
               , "Column `PFG` of `mat.PFG.soil` must contain different values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = "", type = "H", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 3))
               , "`mat.PFG.soil$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = NA, soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 3))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = 2, soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 3))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 3))
               , "`mat.PFG.soil$type` must be either `H`, `C` or `P`", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = NA, soil_tol_min = 2, soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = "a", soil_tol_min = 2, soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = factor(1), soil_tol_min = 2, soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = factor("a"), soil_tol_min = 2, soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = NA, soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = "a", soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = factor(1), soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = factor("a"), soil_tol_max = 3))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = NA))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = "a"))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = factor(1)))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = factor("a")))
               , "Wrong type of data!\n Columns `soil_contrib`, `soil_tol_min` and `soil_tol_max` of `mat.PFG.soil` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = 2, soil_tol_max = 3))
               , "Wrong type of data!\n Column `soil_tol_min` of `mat.PFG.soil` must contain values equal or inferior to `soil_contrib`")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 1, soil_tol_min = 1, soil_tol_max = 0))
               , "Wrong type of data!\n Column `soil_tol_max` of `mat.PFG.soil` must contain values equal or superior to `soil_contrib`")
})

## INPUTS
test_that("PRE_FATE.params_PFGsoil gives error with wrong data : mat.PFG.tol", {
  # if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  # PRE_FATE.skeletonDirectory()
  # expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
  #                                      , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3))
  #              , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = NA)
               , "`mat.PFG.tol` must be a data.frame")
  # expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
  #                                      , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
  #                                      , mat.PFG.tol = NULL)
  #              , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = "")
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = 1)
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = factor(1))
               , "`mat.PFG.tol` must be a data.frame")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = matrix(1))
               , "`mat.PFG.tol` must be a data.frame")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame())
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, soilResources, soil_tol)", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(1))
               , "`mat.PFG.tol` does not have the appropriate number of rows (>0) or columns (PFG, lifeStage, soilResources, soil_tol)", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.tol` must be `PFG`, `lifeStage`, `soilResources` and `soil_tol`")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = "", lifeStage = "Germinant", soilResources = "Low", soil_tol = 2))
               , "`mat.PFG.tol$PFG` must contain a character value of length > 0", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = NA, soilResources = "Low", soil_tol = 2))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = 2, soilResources = "Low", soil_tol = 2))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "", soilResources = "Low", soil_tol = 2))
               , "`mat.PFG.tol$lifeStage` must be either `Germinant`, `Immature` or `Mature`", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = NA, soil_tol = 2))
               , "`mat.PFG.tol$soilResources` must be either `Low`, `Medium` or `High`", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = 2, soil_tol = 2))
               , "`mat.PFG.tol$soilResources` must be either `Low`, `Medium` or `High`", fixed = T)
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "", soil_tol = 2))
               , "`mat.PFG.tol$soilResources` must be either `Low`, `Medium` or `High`", fixed = T)
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = NA))
               , "Wrong type of data!\n Columns `soil_tol` of `mat.PFG.tol` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = "a"))
               , "Wrong type of data!\n Columns `soil_tol` of `mat.PFG.tol` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = factor(1)))
               , "Wrong type of data!\n Columns `soil_tol` of `mat.PFG.tol` must contain numeric values")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = factor("a")))
               , "Wrong type of data!\n Columns `soil_tol` of `mat.PFG.tol` must contain numeric values")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = c(2,NA)))
               , "Columns `soil_tol` of `mat.PFG.tol` must not contain NA values")
  
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = -1))
               , "Column `soil_tol` of `mat.PFG.tol` must contain values between 0 and 10")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = 1.5))
               , "Column `soil_tol` of `mat.PFG.tol` must contain values between 0 and 10")
  expect_error(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                       , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 1, soil_tol_max = 3)
                                       , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = 11))
               , "Column `soil_tol` of `mat.PFG.tol` must contain values between 0 and 10")
})


## OUTPUTS
test_that("PRE_FATE.params_PFGsoil gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 2, soil_tol_max = 3))
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 2, soil_tol_max = 3))
                 , "already exists. It will be replaced.")
  
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 2, soil_tol_max = 3)
                                         , mat.PFG.tol = data.frame(PFG = 1, lifeStage = "Germinant", soilResources = "Low", soil_tol = 9))
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/SOIL_1.txt has been successfully created !")
  
  expect_warning(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 2, soil_tol_max = 3)
                                         , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 2, soil_tol_max = 3)
                                         , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                                         , mat.PFG.soil = data.frame(PFG = 1, type = "H", soil_contrib = 2, soil_tol_min = 2, soil_tol_max = 3)
                                         , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/PFGS/SOIL/scen1/SOIL_1.txt has been successfully created !")
})
