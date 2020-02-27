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
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.no_PFG", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation")
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = NA)
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = NULL)
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = "")
               , "`required.no_PFG` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5)
               , "`required.no_STRATA` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.no_STRATA", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = NA)
               , "`required.no_STRATA` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = NULL)
               , "`required.no_STRATA` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = "")
               , "`required.no_STRATA` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2)
               , "`required.simul_duration` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.simul_duration", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = NA)
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = NULL)
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = "")
               , "`required.simul_duration` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100)
               , "`required.seeding_duration` must be an integer >= 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_duration", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = NA)
               , "`required.seeding_duration` must be an integer >= 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = NULL)
               , "`required.seeding_duration` must be an integer >= 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = "")
               , "`required.seeding_duration` must be an integer >= 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100)
               , "`required.seeding_timestep` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_timestep", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = NA)
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = NULL)
               , "`required.seeding_timestep` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = "")
               , "`required.seeding_timestep` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.seeding_input", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = NA)
               , "`required.seeding_input` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = NULL)
               , "`required.seeding_input` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = "")
               , "`required.seeding_input` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_low", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = NA)
               , "`required.max_abund_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = NULL)
               , "`required.max_abund_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = "")
               , "`required.max_abund_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = -1)
               , "`required.max_abund_low` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_medium", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = NA)
               , "`required.max_abund_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = NULL)
               , "`required.max_abund_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = "")
               , "`required.max_abund_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = -1)
               , "`required.max_abund_medium` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : required.max_abund_high", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = NA)
               , "`required.max_abund_high` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = NULL)
               , "`required.max_abund_high` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = "")
               , "`required.max_abund_high` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = -1)
               , "`required.max_abund_high` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : LIGHT.thresh_medium", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = NA)
               , "`LIGHT.thresh_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = NULL)
               , "`LIGHT.thresh_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = "")
               , "`LIGHT.thresh_medium` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = -1)
               , "`LIGHT.thresh_medium` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : LIGHT.thresh_low", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = NA)
               , "`LIGHT.thresh_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = NULL)
               , "`LIGHT.thresh_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = "")
               , "`LIGHT.thresh_low` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doLight = T
                                                , LIGHT.thresh_medium = 13000000
                                                , LIGHT.thresh_low = -1)
               , "`LIGHT.thresh_low` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DISPERSAL.mode", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDispersal = T
                                                , DISPERSAL.mode = NA)
               , "`DISPERSAL.mode` must be either `1 (uniform kernel)`, `2 (exponential kernel)` or `3 (exponential kernel with probability)`"
               , fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDispersal = T
                                                , DISPERSAL.mode = NULL)
               , "`DISPERSAL.mode` must be either `1 (uniform kernel)`, `2 (exponential kernel)` or `3 (exponential kernel with probability)`"
               , fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDispersal = T
                                                , DISPERSAL.mode = "")
               , "`DISPERSAL.mode` must be either `1 (uniform kernel)`, `2 (exponential kernel)` or `3 (exponential kernel with probability)`"
               , fixed = T)
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : HABSUIT.ref_option", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doHabSuitability = T
                                                , HABSUIT.ref_option = NA)
               , "`HABSUIT.ref_option` must be either `1 (random)` or `2 (distribution per PFG)`", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doHabSuitability = T
                                                , HABSUIT.ref_option = NULL)
               , "`HABSUIT.ref_option` must be either `1 (random)` or `2 (distribution per PFG)`", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doHabSuitability = T
                                                , HABSUIT.ref_option = "")
               , "`HABSUIT.ref_option` must be either `1 (random)` or `2 (distribution per PFG)`", fixed = T)
})



## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DIST.no", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = NA)
               , "`DIST.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = NULL)
               , "`DIST.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = "")
               , "`DIST.no` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2)
               , "`DIST.no_sub` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DIST.no_sub", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = NA)
               , "`DIST.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = NULL)
               , "`DIST.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = "")
               , "`DIST.no_sub` must be an integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2)
               , "`DIST.freq` must be a vector of integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : DIST.freq", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = NA)
               , "`DIST.freq` must be a vector of integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = NULL)
               , "`DIST.freq` must be a vector of integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = "")
               , "`DIST.freq` must be a vector of integer > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = 2)
               , "`DIST.freq` must contain as many values as the number of disturbances (`DIST.no`)", fixed = T)
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doDisturbances = T
                                                , DIST.no = 2
                                                , DIST.no_sub = 2
                                                , DIST.freq = c(2,0))
               , "`DIST.freq` must be a vector of integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : SOIL.init", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = NA)
               , "`SOIL.init` must contain numeric value > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = NULL)
               , "`SOIL.init` must contain numeric value > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = "")
               , "`SOIL.init` must contain numeric value > 0")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = 2)
               , "`SOIL.retention` must contain numeric value between 0 and 1")
})


## INPUTS
test_that("PRE_FATE.params_globalParameters gives error with wrong data : SOIL.retention", {
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = 2
                                                , SOIL.retention = NA)
               , "`SOIL.retention` must contain numeric value between 0 and 1")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = 2
                                                , SOIL.retention = NULL)
               , "`SOIL.retention` must contain numeric value between 0 and 1")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = 2
                                                , SOIL.retention = "")
               , "`SOIL.retention` must contain numeric value between 0 and 1")
  expect_error(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                , required.no_PFG = 5
                                                , required.no_STRATA = 2
                                                , required.simul_duration = 100
                                                , required.seeding_duration = 100
                                                , required.seeding_timestep = 100
                                                , required.seeding_input = 100
                                                , required.max_abund_low = 3000000
                                                , required.max_abund_medium = 5000000
                                                , required.max_abund_high = 9000000
                                                , doSoil = T
                                                , SOIL.init = 2
                                                , SOIL.retention = 2)
               , "`SOIL.retention` must contain numeric value between 0 and 1")
})



## OUTPUTS
test_that("PRE_FATE.params_globalParameters gives correct output", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.no_CPU = "a"
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000)
                 , "`opt.no_CPU` must be an integer > 0", fixed = T)
  
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000)
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V2.txt has been successfully created !")
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000)
                 , "already exists. It will be replaced.")
  
  expect_message(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000
                                                  , doDisturbances = T
                                                  , DIST.no = 2
                                                  , DIST.no_sub = 2
                                                  , DIST.freq = c(2,2))
                 , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V3.txt has been successfully created !")
  
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000.5
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000)
                 , "`required.max_abund_low` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000.5
                                                  , required.max_abund_high = 9000000)
                 , "`required.max_abund_medium` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000.5)
                 , "`required.max_abund_high` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000
                                                  , doLight = TRUE
                                                  , LIGHT.thresh_medium = 100.5
                                                  , LIGHT.thresh_low = 50)
                 , "`LIGHT.thresh_medium` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                                  , opt.replacePrevious = TRUE
                                                  , required.no_PFG = 5
                                                  , required.no_STRATA = 2
                                                  , required.simul_duration = 100
                                                  , required.seeding_duration = 100
                                                  , required.seeding_timestep = 100
                                                  , required.seeding_input = 100
                                                  , required.max_abund_low = 3000000
                                                  , required.max_abund_medium = 5000000
                                                  , required.max_abund_high = 9000000
                                                  , doLight = TRUE
                                                  , LIGHT.thresh_medium = 100
                                                  , LIGHT.thresh_low = 50.5)
                 , "`LIGHT.thresh_low` is a double. It will be converted (rounded) to an integer"
                 , fixed = TRUE)
})
