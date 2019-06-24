library(RFate)
context("PRE_FATE.speciesClustering_step3() function")


## INPUTS
test_that("PRE_FATE.speciesClustering_step3 gives error with missing data", {
  expect_error(PRE_FATE.speciesClustering_step3()
               , "Wrong type of data!\n `mat.species.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(NA)
               , "Wrong type of data!\n `mat.species.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(NULL)
               , "Wrong type of data!\n `mat.species.traits` must be a data.frame")
})

## INPUTS
test_that("PRE_FATE.speciesClustering_step3 gives error with wrong data : mat.species.traits", {
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = 1)
               , "Wrong type of data!\n `mat.species.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = factor(1))
               , "Wrong type of data!\n `mat.species.traits` must be a data.frame")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = matrix(1))
               , "Wrong type of data!\n `mat.species.traits` must be a data.frame")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame())
               , "Wrong dimension(s) of data!\n `mat.species.traits` does not have the appropriate number of rows (>0) or columns (species, PFG, (type), (height), (maturity), (longevity), (dispersal), (light), (soil_contrib), (soil_tol_min), (soil_tol_max), (palatability))"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(1))
               , "Wrong dimension(s) of data!\n `mat.species.traits` does not have the appropriate number of rows (>0) or columns (species, PFG, (type), (height), (maturity), (longevity), (dispersal), (light), (soil_contrib), (soil_tol_min), (soil_tol_max), (palatability))"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(1,2))
               , "Wrong dimension(s) of data!\n `mat.species.traits` does not have the appropriate number of rows (>0) or columns (species, PFG, (type), (height), (maturity), (longevity), (dispersal), (light), (soil_contrib), (soil_tol_min), (soil_tol_max), (palatability))"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(1,2,3))
               , "Wrong type of data!\n Column names of `mat.species.traits` must be `species` and `PFG`")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = NA, trait = 3))
               , "Wrong type of data!\n Columns `PFG` of `mat.species.traits` must not contain NA values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = c(2, NA), trait = 3))
               , "Wrong type of data!\n Columns `PFG` of `mat.species.traits` must not contain NA values")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", type = NA))
               , "Wrong type of data!\n Columns `type` of `mat.species.traits` must not contain NA values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", type = 3))
               , "Wrong type of data!\n `mat.species.traits$type` must be either `H`, `C` or `P`"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", type = "Herb"))
               , "Wrong type of data!\n `mat.species.traits$type` must be either `H`, `C` or `P`"
               , fixed = TRUE)
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", height = NA))
               , "Wrong type of data!\n Columns `height` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", height = factor(1)))
               , "Wrong type of data!\n Columns `height` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", height = "1"))
               , "Wrong type of data!\n Columns `height` of `mat.species.traits` must contain numeric values")

  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", maturity = NA))
               , "Wrong type of data!\n Columns `maturity` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", maturity = factor(1)))
               , "Wrong type of data!\n Columns `maturity` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", maturity = "1"))
               , "Wrong type of data!\n Columns `maturity` of `mat.species.traits` must contain numeric values")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", longevity = NA))
               , "Wrong type of data!\n Columns `longevity` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", longevity = factor(1)))
               , "Wrong type of data!\n Columns `longevity` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", longevity = "1"))
               , "Wrong type of data!\n Columns `longevity` of `mat.species.traits` must contain numeric values")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", soil_contrib = NA))
               , "Wrong type of data!\n Columns `soil_contrib` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", soil_contrib = factor(1)))
               , "Wrong type of data!\n Columns `soil_contrib` of `mat.species.traits` must contain numeric values")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", soil_contrib = "1"))
               , "Wrong type of data!\n Columns `soil_contrib` of `mat.species.traits` must contain numeric values")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A", soil_contrib = 1))
               , "Missing data!\n Column names of `mat.species.traits` must contain `soil_contrib` and `soil_tolerance`")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A"
                                                                                , soil_tolerance = "NA"))
               , "Missing data!\n Column names of `mat.species.traits` must contain `soil_contrib` and `soil_tolerance`")
  
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A"
                                                                                , soil_contrib = 1, soil_tolerance = "NA"))
               , "Wrong type of data!\n Column `soil_tolerance` of `mat.species.traits` must contain values between 1 and 2")
  expect_error(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1, PFG = "A"
                                                                                , soil_contrib = 1, soil_tolerance = 3))
               , "Wrong type of data!\n Column `soil_tolerance` of `mat.species.traits` must contain values between 1 and 2")

})

## OUTPUTS
test_that("PRE_FATE.speciesClustering_step3 gives correct output", {
  expect_output(str(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1
                                                                                      , PFG = "A"
                                                                                      , type = "H"
                                                                                      , height = 10
                                                                                      , maturity = 5
                                                                                      , longevity = 2
                                                                                      , soil_contrib = 0.5, soil_tolerance = 1))), "data.frame")
  expect_output(str(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1
                                                                                     , PFG = "A"
                                                                                     , type = "H"
                                                                                     , height = 10
                                                                                     , maturity = c(5, NA, 10)
                                                                                     , longevity = c(10, 14, NA)
                                                                                     , soil_contrib = 0.5, soil_tolerance = 1))), "data.frame")
  expect_output(str(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1
                                                                                     , PFG = "A"
                                                                                     , type = "H"
                                                                                     , dispersal = 2
                                                                                     , light = 8
                                                                                     , palatability = 1))), "data.frame")
  expect_output(str(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1
                                                                                     , PFG = "A"
                                                                                     , type = "H"
                                                                                     , height = 10
                                                                                     , light = 8))), "data.frame")
  
  
  expect_output(str(PRE_FATE.speciesClustering_step3(mat.species.traits = data.frame(species = 1
                                                                                     , PFG = c("A", "A", "B", "C")
                                                                                     , type = "H"
                                                                                     , height = 10
                                                                                     , maturity = c(NA, NA, 10, NA)
                                                                                     , longevity = c(10, 14, NA, NA)
                                                                                     , soil_contrib = 0.5, soil_tolerance = 1))), "data.frame")
  
})

  
  