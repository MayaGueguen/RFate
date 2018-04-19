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
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,2,3,4,5,6,7,8), ncol=4))
               , "`mat.species.DIST` does not have the same number of rows")
  
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
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,2,3,4,5,6,7,8,9), ncol=3)
                                                     , matrix(c(1,2,3,4,5,6,7,8), ncol=4)))
               , "does not have the same number of rows", fixed = T)
})

## OUTPUTS
test_that("PRE_FATE.speciesClustering_step1 gives right output", {
  expect_output(str(PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))), "List")
  expect_equal(length(PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))), 2)
  
  expect_output(str(PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))$clust.dendograms), "List")
  expect_equal(length(PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))$clust.dendograms), 1)
  
  expect_output(str(PRE_FATE.speciesClustering_step1(list(matrix(seq(9), ncol=3)
                                                          , matrix(seq(9), ncol=3)))$clust.dendograms), "List")
  expect_equal(length(PRE_FATE.speciesClustering_step1(list(matrix(seq(9), ncol=3)
                                                            , matrix(seq(9), ncol=3)))$clust.dendograms), 2)
  
  expect_output(str(PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))$clust.evaluation), "data.frame")
  expect_output(str(PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))$clust.evaluation), "4 variables")
  
  expect_output(str(PRE_FATE.speciesClustering_step1(list(matrix(seq(9), ncol=3)
                                                          , matrix(seq(9), ncol=3)))$clust.evaluation), "data.frame")
  expect_output(str(PRE_FATE.speciesClustering_step1(list(matrix(seq(9), ncol=3)
                                                          , matrix(seq(9), ncol=3)))$clust.evaluation), "4 variables")
})
