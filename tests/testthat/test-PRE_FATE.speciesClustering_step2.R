library(RFate)
context("PRE_FATE.speciesClustering_step2() function")


## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with missing data", {
  expect_error(PRE_FATE.speciesClustering_step2()
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  expect_error(PRE_FATE.speciesClustering_step2(NA)
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  expect_error(PRE_FATE.speciesClustering_step2(NULL)
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
})

## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with wrong data : clust.dendograms", {
  expect_error(PRE_FATE.speciesClustering_step2(1)
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  expect_error(PRE_FATE.speciesClustering_step2("a")
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  expect_error(PRE_FATE.speciesClustering_step2(factor("A"))
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  expect_error(PRE_FATE.speciesClustering_step2(data.frame(1))
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = list())
               , "missing `clust.dendograms` information which must be of class `hclust` or a list `hclust` objects")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = list(1))
               , "each element of `clust.dendograms` must be of class `hclust`")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2))))
               , "missing `no.clusters` information")
})


## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with wrong data : no.clusters", {
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = NA)
               , "missing `no.clusters` information")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = NULL)
               , "missing `no.clusters` information")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = list())
               , "missing `no.clusters` information")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = data.frame(1))
               , "missing `no.clusters` information")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = factor(1))
               , "missing `no.clusters` information")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = "a")
               , "missing `no.clusters` information")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = c(1,2))
               , "Wrong type of data!\n `no.clusters` must have the same length than `clust.dendograms`")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1)
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
})


## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with wrong data : mat.species.DIST", {
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = NA)
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = NULL)
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = 1)
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = "a")
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = factor(1))
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = matrix(seq(4), ncol = 2))
               , "missing `mat.species.DIST` information which must be a dist object, or a list of dist objects")
  
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = list(matrix(seq(4), ncol = 2), 1))
               , "Wrong type of data!\n `mat.species.DIST` must have the same length than `clust.dendograms`")
  
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = as.dist(matrix(seq(4), ncol = 2)))
               , "No determinant species have been selected. Please check your data")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendograms = list(hclust(as.dist(matrix(seq(4), ncol = 2))))
                                                , no.clusters = 1
                                                , mat.species.DIST = list(as.dist(matrix(seq(4), ncol = 2))))
               , "No determinant species have been selected. Please check your data")
})

## OUTPUTS
test_that("PRE_FATE.speciesClustering_step2 gives right output", {
  data(MontBlanc)
  sp.DIST = PRE_FATE.speciesDistance(mat.species.traits = MontBlanc$mat.traits
                                     , mat.species.overlap = MontBlanc$mat.nicheOverlap
                                     , opt.max.percent.NA = 0.9
                                     , opt.max.percent.similarSpecies = 0.25
                                     , opt.min.sd = 0.3)
  sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)

  expect_output(str(PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms[[1]]
                                                , no.clusters = 11
                                                , mat.species.DIST = sp.DIST[[1]])), "List")
  expect_output(str(PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms[[1]]
                                                     , no.clusters = 11
                                                     , mat.species.DIST = sp.DIST[[1]])[[2]]), "10 variables")
  expect_equal(length(PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms[[1]]
                                                     , no.clusters = 11
                                                     , mat.species.DIST = sp.DIST[[1]])), 2)
  
  expect_output(str(PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                                     , no.clusters = c(11, 7, 8)
                                                     , mat.species.DIST = sp.DIST)), "List")
  expect_output(str(PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                                     , no.clusters = c(11, 7, 8)
                                                     , mat.species.DIST = sp.DIST)[[2]]), "10 variables")
  expect_equal(length(PRE_FATE.speciesClustering_step2(clust.dendograms = sp.CLUST$clust.dendograms
                                                       , no.clusters = c(11, 7, 8)
                                                       , mat.species.DIST = sp.DIST)), 2)
})
