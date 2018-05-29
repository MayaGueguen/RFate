library(RFate)
context("PRE_FATE.params_globalParameters() function")

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with missing data", {
  expect_error(PRE_FATE.params_globalParameters()
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(NA)
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_globalParameters(1)
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters("a")
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  expect_error(PRE_FATE.params_globalParameters(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.succ_option", {
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation")
               , "`required.succ_option` must be either `fate` or `fateh` (habitat suitability)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.succ_option = NA)
               , "`required.succ_option` must be either `fate` or `fateh` (habitat suitability)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.succ_option = NULL)
               , "`required.succ_option` must be either `fate` or `fateh` (habitat suitability)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.succ_option = "")
               , "`required.succ_option` must be either `fate` or `fateh` (habitat suitability)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.succ_option = 10)
               , "`required.succ_option` must be either `fate` or `fateh` (habitat suitability)", fixed = T)
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.nb_PFG", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = NA)
               , "`required.nb_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = NULL)
               , "`required.nb_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = "")
               , "`required.nb_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5)
               , "`required.nb_STRATA` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.nb_STRATA", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = NA)
               , "`required.nb_STRATA` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = NULL)
               , "`required.nb_STRATA` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = "")
               , "`required.nb_STRATA` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2)
               , "`required.hs_option` must be either 1 (random) or 2 (distribution per PFG)", fixed = T)
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.hs_option", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = NA)
               , "`required.hs_option` must be either 1 (random) or 2 (distribution per PFG)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = NULL)
               , "`required.hs_option` must be either 1 (random) or 2 (distribution per PFG)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = "")
               , "`required.hs_option` must be either 1 (random) or 2 (distribution per PFG)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1)
               , "`required.seeding_timestep` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_timestep", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = NA)
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = NULL)
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = "")
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100)
               , "`required.seeding_duration` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_duration", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = NA)
               , "`required.seeding_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = NULL)
               , "`required.seeding_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = "")
               , "`required.seeding_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = 100)
               , "`required.simul_duration` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.simul_duration", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = 100
                                                , required.simul_duration = NA)
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = 100
                                                , required.simul_duration = NULL)
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.succ_option = "fate"
                                                , required.nb_PFG = 5
                                                , required.nb_STRATA = 2
                                                , required.hs_option = 1
                                                , required.seeding_timestep = 100
                                                , required.seeding_duration = 100
                                                , required.simul_duration = "")
               , "`required.simul_duration` must be an integer > 0")
})



## OUTPUTS
test_that("PRE_FATE.params_globalParameters gives correct output", {
  PRE_FATE.skeletonDirectory()
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.nb_CPU = "a"
                                                  , required.succ_option = "fate"
                                                  , required.nb_PFG = 5
                                                  , required.nb_STRATA = 2
                                                  , required.hs_option = 1
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_duration = 100
                                                  , required.simul_duration = 100)
               , "`opt.nb_CPU` must be an integer > 0", fixed = T)

  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.succ_option = "fate"
                                                  , required.nb_PFG = 5
                                                  , required.nb_STRATA = 2
                                                  , required.hs_option = 1
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_duration = 100
                                                  , required.simul_duration = 100)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.succ_option = "fate"
                                                  , required.nb_PFG = 5
                                                  , required.nb_STRATA = 2
                                                  , required.hs_option = 1
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_duration = 100
                                                  , required.simul_duration = 100)
                 , "`params.file` already exists. It will be replaced.")
})
