library(RFate)
context("PRE_FATE.params_multipleSet() function")

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with missing data", {
  expect_error(PRE_FATE.params_multipleSet()
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(NA)
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(NULL)
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
})


## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : name.simulation.1", {
  expect_error(PRE_FATE.params_multipleSet(1)
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet("a")
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(factor(1))
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(matrix(seq(2), ncol=2))
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "`name.simulation.1` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "`name.simulation.1` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation/DATA")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "`name.simulation.1` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  file.create("FATE_simulation/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : file.simulParam.1", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = NA)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = 1)
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = factor("a"))
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "a")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/a` does not exist")
  
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : file.simulParam.2", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = NA)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = 1)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = factor("a"))
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "a")
               , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/a` does not exist")
  
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt")
               , "You must select different simulation parameter files !")
  file.create("FATE_simulation/PARAM_SIMUL/toto2.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto2.txt")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : name.simulation.2", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , name.simulation.2 = "a")
               , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ contain one or more files.")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , name.simulation.2 = "a")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "a")
               , "`name.simulation.2` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = factor(1))
               , "You must select different simulation parameter files !")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = matrix(seq(2), ncol=2))
               , "You must select different simulation parameter files !")
  
  
  if (dir.exists("FATE_simulation2")) unlink("FATE_simulation2", recursive = TRUE)
  dir.create("FATE_simulation2")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "`name.simulation.2` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation2/PARAM_SIMUL")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "`name.simulation.2` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation2/DATA")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "`name.simulation.2` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation2/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "Wrong name file given!\n `FATE_simulation2/PARAM_SIMUL/toto.txt` does not exist")
  
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation")
               , "You must select different simulation parameter files !")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto2.txt"
                                           , name.simulation.2 = "FATE_simulation")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  file.create("FATE_simulation2/PARAM_SIMUL/toto.txt")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , file.simulParam.2 = "toto.txt"
                                           , name.simulation.2 = "FATE_simulation2")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})


## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : no_simulations", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = NA)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = NULL)
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = factor(1))
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = "")
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = factor("a"))
               , "Wrong type of data!\n `no_simulations` must be an integer > 0")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_max", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_max = NA)
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_max = NULL)
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_max = factor(1))
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_max = -2)
               , "Wrong data given!\n `opt.percent_max` must be between 0 and 1")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_max = 1.01)
               , "Wrong data given!\n `opt.percent_max` must be between 0 and 1")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_seeding", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = NA)
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = NULL)
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = factor(1))
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = -2)
               , "Wrong data given!\n `opt.percent_seeding` must be between 0 and 1")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_seeding = 1.01)
               , "Wrong data given!\n `opt.percent_seeding` must be between 0 and 1")
})

## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : opt.percent_light", {
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = NA)
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = NULL)
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = factor(1))
               , "Wrong data given!\n `opt.percent_max`, `opt.percent_seeding` and `opt.percent_light` must contain numeric values")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = -2)
               , "Wrong data given!\n `opt.percent_light` must be between 0 and 1")
  expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                           , file.simulParam.1 = "toto.txt"
                                           , no_simulations = 10
                                           , opt.percent_light = 1.01)
               , "Wrong data given!\n `opt.percent_light` must be between 0 and 1")
})


## INPUTS
test_that("PRE_FATE.params_multipleSet gives error with wrong data : within file.simulParam.1", {
expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
                                         , file.simulParam.1 = "toto.txt"
                                         , no_simulations = 10)
             , "The file FATE_simulation/PARAM_SIMUL/toto.txt is empty. Please check.")
  
})



# ## INPUTS
# test_that("PRE_FATE.params_multipleSet gives error with wrong data : DISPERSAL.mode", {
#   expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                 , required.no_PFG = 5
#                                                 , required.no_STRATA = 2
#                                                 , required.simul_duration = 100
#                                                 , required.seeding_duration = 100
#                                                 , required.seeding_timestep = 100
#                                                 , required.seeding_input = 100
#                                                 , required.max_by_cohort = 5000000
#                                                 , required.max_abund_low = 3000000
#                                                 , required.max_abund_medium = 5000000
#                                                 , required.max_abund_high = 9000000
#                                                 , doDispersal = T
#                                                 , DISPERSAL.mode = NA)
#                , "`DISPERSAL.mode` must be either `1 (uniform kernel)`, `2 (exponential kernel)` or `3 (exponential kernel with probability)`"
#                , fixed = T)
#   expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                 , required.no_PFG = 5
#                                                 , required.no_STRATA = 2
#                                                 , required.simul_duration = 100
#                                                 , required.seeding_duration = 100
#                                                 , required.seeding_timestep = 100
#                                                 , required.seeding_input = 100
#                                                 , required.max_by_cohort = 5000000
#                                                 , required.max_abund_low = 3000000
#                                                 , required.max_abund_medium = 5000000
#                                                 , required.max_abund_high = 9000000
#                                                 , doDispersal = T
#                                                 , DISPERSAL.mode = NULL)
#                , "`DISPERSAL.mode` must be either `1 (uniform kernel)`, `2 (exponential kernel)` or `3 (exponential kernel with probability)`"
#                , fixed = T)
#   expect_error(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                 , required.no_PFG = 5
#                                                 , required.no_STRATA = 2
#                                                 , required.simul_duration = 100
#                                                 , required.seeding_duration = 100
#                                                 , required.seeding_timestep = 100
#                                                 , required.seeding_input = 100
#                                                 , required.max_by_cohort = 5000000
#                                                 , required.max_abund_low = 3000000
#                                                 , required.max_abund_medium = 5000000
#                                                 , required.max_abund_high = 9000000
#                                                 , doDispersal = T
#                                                 , DISPERSAL.mode = "")
#                , "`DISPERSAL.mode` must be either `1 (uniform kernel)`, `2 (exponential kernel)` or `3 (exponential kernel with probability)`"
#                , fixed = T)
# })
# 
# ## OUTPUTS
# test_that("PRE_FATE.params_multipleSet gives correct output", {
#   if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
#   PRE_FATE.skeletonDirectory()
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.no_CPU = "a"
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000)
#                  , "`opt.no_CPU` must be an integer > 0", fixed = T)
#   
#   expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000)
#                  , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V2.txt has been successfully created !")
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000)
#                  , "already exists. It will be replaced.")
#   
#   expect_message(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000
#                                                   , doDisturbances = T
#                                                   , DIST.no = 2
#                                                   , DIST.no_sub = 2
#                                                   , DIST.freq = c(2,2))
#                  , "The parameter file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V3.txt has been successfully created !")
#   
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000.5
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000)
#                  , "`required.max_by_cohort` is a double. It will be converted (rounded) to an integer"
#                  , fixed = TRUE)
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000.5
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000)
#                  , "`required.max_abund_low` is a double. It will be converted (rounded) to an integer"
#                  , fixed = TRUE)
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000.5
#                                                   , required.max_abund_high = 9000000)
#                  , "`required.max_abund_medium` is a double. It will be converted (rounded) to an integer"
#                  , fixed = TRUE)
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000.5)
#                  , "`required.max_abund_high` is a double. It will be converted (rounded) to an integer"
#                  , fixed = TRUE)
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000
#                                                   , doLight = TRUE
#                                                   , LIGHT.thresh_medium = 100.5
#                                                   , LIGHT.thresh_low = 50)
#                  , "`LIGHT.thresh_medium` is a double. It will be converted (rounded) to an integer"
#                  , fixed = TRUE)
#   expect_warning(PRE_FATE.params_multipleSet(name.simulation.1 = "FATE_simulation"
#                                                   , opt.replacePrevious = TRUE
#                                                   , required.no_PFG = 5
#                                                   , required.no_STRATA = 2
#                                                   , required.simul_duration = 100
#                                                   , required.seeding_duration = 100
#                                                   , required.seeding_timestep = 100
#                                                   , required.seeding_input = 100
#                                                   , required.max_by_cohort = 5000000
#                                                   , required.max_abund_low = 3000000
#                                                   , required.max_abund_medium = 5000000
#                                                   , required.max_abund_high = 9000000
#                                                   , doLight = TRUE
#                                                   , LIGHT.thresh_medium = 100
#                                                   , LIGHT.thresh_low = 50.5)
#                  , "`LIGHT.thresh_low` is a double. It will be converted (rounded) to an integer"
#                  , fixed = TRUE)
# })
