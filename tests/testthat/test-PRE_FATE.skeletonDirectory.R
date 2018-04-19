library(RFate)
context("PRE_FATE.skeletonDirectory() function")

## INPUTS
test_that("PRE_FATE.skeletonDirectory gives message / warning / error with missing data", {
  expect_error(PRE_FATE.skeletonDirectory(NA), "No data given!\n (missing `name.simulation`)", fixed = T)
  expect_error(PRE_FATE.skeletonDirectory(NULL), "No data given!\n (missing `name.simulation`)", fixed = T)
  expect_error(PRE_FATE.skeletonDirectory(1), "`name.simulation` must contain a character value")
  expect_error(PRE_FATE.skeletonDirectory(factor("a")), "`name.simulation` must contain a character value")
  expect_error(PRE_FATE.skeletonDirectory(factor(1)), "`name.simulation` must contain a character value")
})


## OUTPUTS
test_that("PRE_FATE.skeletonDirectory gives correct output", {
  expect_message(PRE_FATE.skeletonDirectory(), "Your directory tree for your FATE-HD simulation")
  expect_warning(PRE_FATE.skeletonDirectory(), "Directory already exists!")
})
