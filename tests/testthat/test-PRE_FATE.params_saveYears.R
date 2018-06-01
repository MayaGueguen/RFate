library(RFate)
context("PRE_FATE.params_saveYears() function")

## INPUTS
test_that("PRE_FATE.params_saveYears gives error with missing data", {
  expect_error(PRE_FATE.params_saveYears()
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  expect_error(PRE_FATE.params_saveYears(NA)
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  expect_error(PRE_FATE.params_saveYears(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
})


## INPUTS
test_that("PRE_FATE.params_saveYears gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_saveYears(1)
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  expect_error(PRE_FATE.params_saveYears("a")
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  expect_error(PRE_FATE.params_saveYears(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  expect_error(PRE_FATE.params_saveYears(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
})


## INPUTS
test_that("PRE_FATE.params_saveYears gives error with wrong data : years.maps and years.objects", {
  PRE_FATE.skeletonDirectory()
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation")
               , "Both `years.maps` and `years.objects` parameters are NULL. No parameter file will be created")
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = NULL)
               , "Both `years.maps` and `years.objects` parameters are NULL. No parameter file will be created")
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = NULL)
               , "Both `years.maps` and `years.objects` parameters are NULL. No parameter file will be created")
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = NULL, years.objects = NULL)
                 , "Both `years.maps` and `years.objects` parameters are NULL. No parameter file will be created")
  
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = NA)
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = "")
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = factor(1))
               , "years.maps` and/or `years.objects` must contain numeric values")

  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = NA)
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = "")
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = factor(1))
               , "years.maps` and/or `years.objects` must contain numeric values")
  
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1, years.objects = NA)
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1, years.objects = "")
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1, years.objects = factor(1))
               , "years.maps` and/or `years.objects` must contain numeric values")
  
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = NA, years.objects = 1)
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = "", years.objects = 1)
               , "years.maps` and/or `years.objects` must contain numeric values")
  expect_error(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = factor(1), years.objects = 1)
               , "years.maps` and/or `years.objects` must contain numeric values")
  
  

})


## OUTPUTS
test_that("PRE_FATE.params_saveYears gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1)
               , "The parameter file FATE_simulation/DATA/SAVE/SAVE_YEARS_maps.txt has been successfully created !")
  expect_message(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = 1)
                 , "The parameter file FATE_simulation/DATA/SAVE/SAVE_YEARS_objects.txt has been successfully created !")
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1)
                 , "`params.file` already exists. It will be replaced.")
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = 1)
                 , "`params.file` already exists. It will be replaced.")
  
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1, years.objects = 1
                                           , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1, years.objects = 1
                                           , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.maps = 1, opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/SAVE/scen1/SAVE_YEARS_maps.txt has been successfully created !")
  expect_message(PRE_FATE.params_saveYears(name.simulation = "FATE_simulation", years.objects = 1, opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/SAVE/scen1/SAVE_YEARS_objects.txt has been successfully created !")
})
