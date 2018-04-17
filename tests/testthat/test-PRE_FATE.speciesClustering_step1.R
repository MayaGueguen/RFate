library(RFate)
context("PRE_FATE.speciesClustering_step1() function")


## INPUT
test_that("PRE_FATE.speciesClustering_step1 gives error with missing data", {
  expect_error(PRE_FATE.speciesClustering_step1(), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(NULL), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(NA), "must be a dissimilarity object", fixed = T)
})

## INPUT
test_that("PRE_FATE.speciesClustering_step1 gives error with wrong data", {
  expect_error(PRE_FATE.speciesClustering_step1(1), "must be a dissimilarity object", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1("a"), "must be a dissimilarity object", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(factor("A")), "must be a dissimilarity object", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(data.frame(1)), "must be a dissimilarity object", fixed = T)
  
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,5,2,2), ncol=2))
               , "All clustering methods (maybe for a specific group) give NA values for Mouchet measure", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,2,3,4,NA,6,7,8,9), ncol=3)), "`mat.species.DIST` contain NA values ")
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,2,3,NA,NA,6,7,8,9), ncol=3)), "`mat.species.DIST` contain NA values ")
  
  expect_error(PRE_FATE.speciesClustering_step1(list()), "must be of length > 0")
  expect_error(PRE_FATE.speciesClustering_step1(list(NA)), "must be a dissimilarity object")
  expect_error(PRE_FATE.speciesClustering_step1(list(1)), "must be a dissimilarity object")
  expect_error(PRE_FATE.speciesClustering_step1(list("a")), "must be a dissimilarity object")
  expect_error(PRE_FATE.speciesClustering_step1(list(list())), "must be a dissimilarity object")
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,5,2,2), ncol=2)))
               , "All clustering methods (maybe for a specific group) give NA values for Mouchet measure", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,2,3,4,NA,6,7,8,9), ncol=3)))
               , "`mat.species.DIST` contain NA values ")
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,2,3,4,5,6,7,8,9), ncol=3)
                                                     , matrix(c(1,5,2,2), ncol=2)))
               , "All clustering methods (maybe for a specific group) give NA values for Mouchet measure", fixed = T)
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,2,3,4,5,6,7,8,9), ncol=3)
                                                     , matrix(c(1,2,3,4,NA,6,7,8,9), ncol=3)))
               , "`mat.species.DIST` contain NA values ", fixed = T)
})

# ## OUTPUTS
# test_that("PRE_FATE.speciesClustering_step1 of BB values give right results", {
#   expect_equal(PRE_FATE.speciesClustering_step1("r"), 0.5)
#   expect_equal(PRE_FATE.speciesClustering_step1("+"), 0.5)
#   expect_equal(PRE_FATE.speciesClustering_step1("1"), 3)
#   expect_equal(PRE_FATE.speciesClustering_step1("2"), 15)
#   expect_equal(PRE_FATE.speciesClustering_step1("3"), 37.5)
#   expect_equal(PRE_FATE.speciesClustering_step1("4"), 62.5)
#   expect_equal(PRE_FATE.speciesClustering_step1("5"), 87.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("r")), 0.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("+")), 0.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("1")), 3)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("2")), 15)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("3")), 37.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("4")), 62.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor("5")), 87.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(1), 3)
#   expect_equal(PRE_FATE.speciesClustering_step1(2), 15)
#   expect_equal(PRE_FATE.speciesClustering_step1(3), 37.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(4), 62.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(5), 87.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor(1)), 3)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor(2)), 15)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor(3)), 37.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor(4)), 62.5)
#   expect_equal(PRE_FATE.speciesClustering_step1(factor(5)), 87.5)
# })
# 
# ## OUTPUTS
# test_that("PRE_FATE.speciesClustering_step1 of none BB values give NA", {
#   expect_equal(PRE_FATE.speciesClustering_step1(-1), as.numeric(NA))
#   expect_equal(PRE_FATE.speciesClustering_step1(6), as.numeric(NA))
#   expect_equal(PRE_FATE.speciesClustering_step1(19856394), as.numeric(NA))
#   expect_equal(PRE_FATE.speciesClustering_step1("a"), as.numeric(NA))
#   expect_equal(PRE_FATE.speciesClustering_step1("abc"), as.numeric(NA))
# })
# 
# ## OUTPUTS
# test_that("PRE_FATE.speciesClustering_step1 of missing give NA", {
#   expect_equal(PRE_FATE.speciesClustering_step1(NA), as.numeric(NA))
#   expect_equal(PRE_FATE.speciesClustering_step1("NA"), as.numeric(NA))
# })
# 
# ## OUTPUTS
# test_that("PRE_FATE.speciesClustering_step1 does not modify length", {
#   expect_equal(length(PRE_FATE.speciesClustering_step1(NA)), 1)
#   expect_equal(length(PRE_FATE.speciesClustering_step1("NA")), 1)
#   expect_equal(length(PRE_FATE.speciesClustering_step1("1")), 1)
#   expect_equal(length(PRE_FATE.speciesClustering_step1(c("1", NA))), 2)
#   expect_equal(length(PRE_FATE.speciesClustering_step1(c("1", "NA"))), 2)
#   expect_equal(length(PRE_FATE.speciesClustering_step1(c(NA, NA))), 2)
#   expect_equal(length(PRE_FATE.speciesClustering_step1(c(12, -789))), 2)
# })
# 
# ## OUTPUTS
# test_that("PRE_FATE.speciesClustering_step1 gives numeric output", {
#   expect_output(str(PRE_FATE.speciesClustering_step1(NA)), "num")
#   expect_output(str(PRE_FATE.speciesClustering_step1(1)), "num")
#   expect_output(str(PRE_FATE.speciesClustering_step1("1")), "num")
#   expect_output(str(PRE_FATE.speciesClustering_step1(c(1, NA))), "num")
# })


