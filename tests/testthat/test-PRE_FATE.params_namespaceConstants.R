library(RFate)
context("PRE_FATE.params_namespaceConstants() function")

## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with missing data", {
  expect_error(PRE_FATE.params_namespaceConstants()
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  expect_error(PRE_FATE.params_namespaceConstants(NA)
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  expect_error(PRE_FATE.params_namespaceConstants(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_namespaceConstants(1)
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  expect_error(PRE_FATE.params_namespaceConstants("a")
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  expect_error(PRE_FATE.params_namespaceConstants(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
  expect_error(PRE_FATE.params_namespaceConstants(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/NAMESPACE_CONSTANTS/ folder")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : global.abund.low", {
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation")
               , "`global.abund.low` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = NA)
               , "`global.abund.low` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = NULL)
               , "`global.abund.low` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = "")
               , "`global.abund.low` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10)
               , "`global.abund.med` must be an integer")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : global.abund.med", {
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = NA)
               , "`global.abund.med` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = NULL)
               , "`global.abund.med` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = "")
               , "`global.abund.med` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5)
               , "`global.abund.high` must be an integer")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : global.abund.high", {
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = NA)
               , "`global.abund.high` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = NULL)
               , "`global.abund.high` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = "")
               , "`global.abund.high` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2)
               , "`global.max.by.cohort` must be an integer")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : global.max.by.cohort", {
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = NA)
               , "`global.max.by.cohort` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = NULL)
               , "`global.max.by.cohort` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = "")
               , "`global.max.by.cohort` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4)
               , "`global.resource.thresh.med` must be an integer")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : global.resource.thresh.med", {
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = NA)
               , "`global.resource.thresh.med` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = NULL)
               , "`global.resource.thresh.med` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = "")
               , "`global.resource.thresh.med` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = 100)
               , "`global.resource.thresh.low` must be an integer")
})


## INPUTS
test_that("PRE_FATE.params_namespaceConstants gives error with wrong data : global.resource.thresh.low", {
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = 100
                                                  , global.resource.thresh.low = NA)
               , "`global.resource.thresh.low` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = 100
                                                  , global.resource.thresh.low = NULL)
               , "`global.resource.thresh.low` must be an integer")
  expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                  , global.abund.low = 10
                                                  , global.abund.med = 5
                                                  , global.abund.high = 2
                                                  , global.max.by.cohort = 4
                                                  , global.resource.thresh.med = 100
                                                  , global.resource.thresh.low = "")
               , "`global.resource.thresh.low` must be an integer")
  # expect_error(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
  #                                                 , global.abund.low = 10
  #                                                 , global.abund.med = 5
  #                                                 , global.abund.high = 2
  #                                                 , global.max.by.cohort = 4
  #                                                 , global.resource.thresh.med = 100
  #                                                 , global.resource.thresh.low = 200)
  #              , "`global.resource.thresh.low` must be an integer")
})


## OUTPUTS
test_that("PRE_FATE.params_namespaceConstants gives correct output", {
  PRE_FATE.skeletonDirectory()
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10.1
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200)
               , "`global.abund.low` is a double. It will be converted (rounded) to an integer", fixed = T)
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5.1
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200)
                 , "`global.abund.med` is a double. It will be converted (rounded) to an integer", fixed = T)
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2.1
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200)
                 , "`global.abund.high` is a double. It will be converted (rounded) to an integer", fixed = T)
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4.1
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200)
                 , "`global.max.by.cohort` is a double. It will be converted (rounded) to an integer", fixed = T)
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100.1
                                                    , global.resource.thresh.low = 200)
                 , "`global.resource.thresh.med` is a double. It will be converted (rounded) to an integer", fixed = T)
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200.1)
                 , "`global.resource.thresh.low` is a double. It will be converted (rounded) to an integer", fixed = T)
  
  expect_message(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200)
                 , "The parameter file FATE_simulation/DATA/NAMESPACE_CONSTANTS/Namespace_constants_V1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_namespaceConstants(name.simulation = "FATE_simulation"
                                                    , global.abund.low = 10
                                                    , global.abund.med = 5
                                                    , global.abund.high = 2
                                                    , global.max.by.cohort = 4
                                                    , global.resource.thresh.med = 100
                                                    , global.resource.thresh.low = 200)
                 , "`params.file` already exists. It will be replaced.")
})
