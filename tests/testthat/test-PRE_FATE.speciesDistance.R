library(RFate)
context("PRE_FATE.speciesDistance() function")

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with missing data", {
  expect_error(PRE_FATE.speciesDistance(), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesDistance(NA), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesDistance(NULL), "No data given!\n (missing", fixed = T)

  expect_error(PRE_FATE.speciesDistance(mat.species.traits = NA), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = NA, mat.species.overlap = NA), "must be a data.frame", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.overlap = NA), "No data given!\n (missing", fixed = T)
})


## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : mat.species.traits", {  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = list(1,NA)), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = matrix(0, ncol = 1)
                                        , mat.species.overlap = matrix(seq(9), ncol = 3))
               , "must be a data.frame")
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(1)
                                        , mat.species.overlap = matrix(seq(9), ncol = 3))
               , "does not have the appropriate number of cols (>=3, at least 2 traits)", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(1, 2, 3)
                                        , mat.species.overlap = matrix(seq(9), ncol = 3))
               , "must contain a column whose name is `species`")
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = 1)
                                        , mat.species.overlap = matrix(seq(9), ncol = 3))
               , "does not have the appropriate number of cols (>=3, at least 2 traits)", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = 1, 2, 3)
                                        , mat.species.overlap = matrix(seq(9), ncol = 3))
               , "does not have the appropriate number of rows (>=2)", fixed = T)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : mat.species.overlap", {  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = 1)
               , "must be either a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)", fixed = T)
  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = data.frame(1))
               , "must contain a column whose name is `species`", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = data.frame(species = 1))
               , "must contain a column whose name is `raster`", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = data.frame(species = 1, raster = 1))
               , "must contain file names which exist", fixed = T)
  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(seq(2), ncol=2))
               , "Wrong dimension(s) of data!\n `mat.species.overlap` does not have the same number of rows", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(seq(2), ncol=1))
               , "Wrong dimension(s) of data!\n `mat.species.overlap` does not have the same number of rows", fixed = T)

})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : min.info.thresh", {
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , min.info.thresh = "a")
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , min.info.thresh = factor(1))
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , min.info.thresh = 1.1)
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , min.info.thresh = -1.1)
               , "must be a number between 0 and 1", fixed = T)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with missing data : mat.species.traits", {  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), NA, 3)
                                        , mat.species.overlap = matrix(1)
                                        , min.info.thresh = 1)
               , "`mat.species.traits` contain trait with too many missing values", fixed = T)
})


## OUTPUTS
test_that("PRE_FATE.speciesDistance gives correct output", {
  expect_output(str(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c")
                                                                             , TRAIT_1 = 1:3
                                                                             , TRAIT_2 = 5:7
                                                                             , GROUP = c("A", "A", "A"))
                                             , mat.species.overlap = matrix(runif(9)
                                                                            , ncol = 3
                                                                            , dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
                                             , opt.traits.selection = c(1, 0))), "dist")
  expect_output(str(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c")
                                                                             , TRAIT_1 = 1:3
                                                                             , TRAIT_2 = 5:7
                                                                             , GROUP = c("A", "A", "A"))
                                             , mat.species.overlap = as.dist(matrix(runif(9)
                                                                            , ncol = 3
                                                                            , dimnames = list(c("a", "b", "c"), c("a", "b", "c"))))
                                             , opt.traits.selection = c(1, 0))), "dist")
  
  expect_output(str(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                             , TRAIT_1 = 1:5
                                                                             , TRAIT_2 = 5:9
                                                                             , GROUP = c("A", "A", "A", "B", "B"))
                                             , mat.species.overlap = matrix(runif(25)
                                                                            , ncol = 5
                                                                            , dimnames = list(c("a", "b", "c", "d", "e")
                                                                                              , c("a", "b", "c", "d", "e")))
                                             , opt.traits.selection = c(1, 0))), "List")
  expect_equal(length(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                             , TRAIT_1 = 1:5
                                                                             , TRAIT_2 = 5:9
                                                                             , GROUP = c("A", "A", "A", "B", "B"))
                                             , mat.species.overlap = matrix(runif(25)
                                                                            , ncol = 5
                                                                            , dimnames = list(c("a", "b", "c", "d", "e")
                                                                                              , c("a", "b", "c", "d", "e")))
                                             , opt.traits.selection = c(1, 0))), 2)
})
