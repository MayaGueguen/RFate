library(RFate)
context("PRE_FATE.params_simulParameters() function")

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with missing data", {
  expect_error(PRE_FATE.params_simulParameters()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_simulParameters(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.simulation", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  system("mkdir FATE_simulation")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  system("mkdir FATE_simulation/PARAM_SIMUL")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  system("mkdir FATE_simulation/DATA")
  system("mkdir FATE_simulation/DATA/NAMESPACE_CONSTANTS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  system("mkdir FATE_simulation/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  system("mkdir FATE_simulation/DATA/SAVE")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/MASK/ folder")
  system("mkdir FATE_simulation/DATA/MASK")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  system("mkdir FATE_simulation/DATA/PFGS")
  system("mkdir FATE_simulation/DATA/PFGS/SUCC")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  system("mkdir FATE_simulation/DATA/PFGS/DISP")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/ENVSUIT/ folder")
  system("mkdir FATE_simulation/DATA/PFGS/ENVSUIT")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  system("mkdir FATE_simulation/RESULTS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
})


## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.mask", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = NULL)
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = NA)
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.mask = "mask")
               , "`name.mask` does not exist or is not inside the DATA/MASK/ folder")
})





# ## OUTPUTS
# test_that("PRE_FATE.params_simulParameters gives correct output", {
#   if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
#   PRE_FATE.skeletonDirectory()
#   expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", type.changing = "DIST"
#                                                  , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B")))
#                  , "The parameter file FATE_simulation/DATA/SCENARIO/DIST_changing_times.txt has been successfully created !")
#   expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", type.changing = "DIST"
#                                                  , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B")))
#                  , "The parameter file FATE_simulation/DATA/SCENARIO/DIST_changing_masks_t10.txt has been successfully created !")
#   expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", type.changing = "DIST"
#                                                  , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B")))
#                  , "already exists. It will be replaced.")
#   
# })
