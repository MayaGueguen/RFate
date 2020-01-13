library(RFate)
library(raster)
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
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = rep(1,2), 2, 3)
                                        , mat.species.overlap = matrix(seq(9), ncol = 3))
               , "Column `species` of `mat.species.traits` must contain different values", fixed = T)
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
  
  file.create("a.txt")
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                        , TRAIT_1 = 1:5
                                                                        , TRAIT_2 = 5:9
                                                                        , GROUP = c("A", "A", "A", "B", "B"))
                                        , mat.species.overlap = data.frame(species = "A", raster = "a.txt"))
               , "`mat.species.overlap` does not have the appropriate number of rows (>=2)", fixed = TRUE)
  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                        , TRAIT_1 = 1:5
                                                                        , TRAIT_2 = 5:9
                                                                        , GROUP = c("A", "A", "A", "B", "B"))
                                        , mat.species.overlap = data.frame(species = rep("A", 2), raster = "a.txt"))
               , "Column `species` of `mat.species.overlap` must contain different values", fixed = TRUE)
  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                        , TRAIT_1 = 1:5
                                                                        , TRAIT_2 = 5:9
                                                                        , GROUP = c("A", "A", "A", "B", "B"))
                                        , mat.species.overlap = data.frame(species = c("A", "B"), raster = "a.txt"))
               , "must contain file names with appropriate extension (`.tif`, `.img`, `.asc`)", fixed = TRUE)
  file.create("a.tif")
  # expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
  #                                                                       , TRAIT_1 = 1:5
  #                                                                       , TRAIT_2 = 5:9
  #                                                                       , GROUP = c("A", "A", "A", "B", "B"))
  #                                       , mat.species.overlap = data.frame(species = c("A", "B"), raster = "a.tif"))
  #              , "Cannot create a RasterLayer object from this file.")
  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(seq(2), ncol=2))
               , "Wrong dimension(s) of data!\n `mat.species.overlap` does not have the same number of rows", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(seq(2), ncol=1))
               , "Wrong dimension(s) of data!\n `mat.species.overlap` does not have the same number of rows", fixed = T)
  
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : opt.max.percent.NA", {
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.NA = "a")
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.NA = factor(1))
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.NA = 1.1)
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.NA = -1.1)
               , "must be a number between 0 and 1", fixed = T)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : opt.max.percent.similarSpecies", {
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.similarSpecies = "a")
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.similarSpecies = factor(1))
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.similarSpecies = 1.1)
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.max.percent.similarSpecies = -1.1)
               , "must be a number between 0 and 1", fixed = T)
})

## INPUTS
test_that("PRE_FATE.speciesDistance gives error with wrong data : opt.min.sd", {
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.min.sd = "a")
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.min.sd = factor(1))
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.min.sd = 1.1)
               , "must be a number between 0 and 1", fixed = T)
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("A", "B"), 2, 3)
                                        , mat.species.overlap = matrix(1)
                                        , opt.min.sd = -1.1)
               , "must be a number between 0 and 1", fixed = T)
})


## INPUTS
# test_that("PRE_FATE.speciesDistance gives error with missing data : mat.species.traits", {
#   expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c")
#                                                                         , TRAIT_1 = 1:3
#                                                                         , TRAIT_2 = c(NA, 6:7)
#                                                                         , GROUP = c("A", "A", "A"))
#                                         , mat.species.overlap = matrix(runif(9)
#                                                                        , ncol = 3
#                                                                        , dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
#                                         , opt.max.percent.NA = 0.3)
#                , "`mat.species.traits` contains traits with too many missing values", fixed = T)
# })


## OUTPUTS
test_that("PRE_FATE.speciesDistance gives correct output", {
  expect_output(str(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c")
                                                                             , TRAIT_1 = 1:3
                                                                             , TRAIT_2 = 5:7
                                                                             , GROUP = c("A", "A", "A"))
                                             , mat.species.overlap = matrix(runif(9)
                                                                            , ncol = 3
                                                                            , dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
                                             , opt.max.percent.NA = 1
                                             , opt.max.percent.similarSpecies = 0.25
                                             , opt.min.sd = 0.3)), "dist")
  expect_output(str(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c")
                                                                             , TRAIT_1 = 1:3
                                                                             , TRAIT_2 = 5:7
                                                                             , GROUP = c("A", "A", "A"))
                                             , mat.species.overlap = as.dist(matrix(runif(9)
                                                                                    , ncol = 3
                                                                                    , dimnames = list(c("a", "b", "c"), c("a", "b", "c"))))
                                             , opt.max.percent.NA = 1
                                             , opt.max.percent.similarSpecies = 0.25
                                             , opt.min.sd = 0.3)), "dist")
  
  expect_output(str(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                             , TRAIT_1 = 1:5
                                                                             , TRAIT_2 = 5:9
                                                                             , GROUP = c("A", "A", "A", "B", "B"))
                                             , mat.species.overlap = matrix(runif(25)
                                                                            , ncol = 5
                                                                            , dimnames = list(c("a", "b", "c", "d", "e")
                                                                                              , c("a", "b", "c", "d", "e")))
                                             , opt.max.percent.NA = 1
                                             , opt.max.percent.similarSpecies = 0.25
                                             , opt.min.sd = 0.3)), "List")
  expect_equal(length(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                               , TRAIT_1 = 1:5
                                                                               , TRAIT_2 = 5:9
                                                                               , GROUP = c("A", "A", "A", "B", "B"))
                                               , mat.species.overlap = matrix(runif(25)
                                                                              , ncol = 5
                                                                              , dimnames = list(c("a", "b", "c", "d", "e")
                                                                                                , c("a", "b", "c", "d", "e")))
                                               , opt.max.percent.NA = 1
                                               , opt.max.percent.similarSpecies = 0.25
                                               , opt.min.sd = 0.3)), 2)
})

## OUTPUTS
test_that("PRE_FATE.speciesDistance gives correct output : with mat.species.overlap", {
  
  PNE_RESULTS = .loadData("PNE_RESULTS")
  PFG.names = names(PNE_RESULTS$abund_str.equilibrium)
  PFG.names = sub("PNE_year_800_", "", PFG.names)
  PFG.names = sapply(PFG.names, function(x) strsplit(x, "_")[[1]][1])
  for (pfg in PFG.names[1])
  {
    ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
    stk = PNE_RESULTS$abund_str.equilibrium[[ind]]
    ras = sum(stk)
    writeRaster(ras
                , filename = paste0("Abund_YEAR_800_", pfg, "_STRATA_all.tif")
                , overwrite = TRUE)
  }
  
  expect_equal(length(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                               , TRAIT_1 = 1:5
                                                                               , TRAIT_2 = 5:9
                                                                               , GROUP = c("A", "A", "A", "B", "B"))
                                               , mat.species.overlap = data.frame(species = c("a", "b", "d", "e")
                                                                                  , raster = "Abund_YEAR_800_C1_STRATA_all.tif")
  )), 2)
  
  expect_error(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                        , TRAIT_1 = 1:5
                                                                        , TRAIT_2 = 5:9
                                                                        , GROUP = c("A", "A", "A", "B", "B"))
                                        , mat.species.overlap = data.frame(species = c("a", "d")
                                                                           , raster = "Abund_YEAR_800_C1_STRATA_all.tif"))
               , "`mat.species.traits` does not have the appropriate number of rows (>=2)", fixed = TRUE)
  
  expect_warning(PRE_FATE.speciesDistance(mat.species.traits = data.frame(species = c("a", "b", "c", "d", "e")
                                                                          , TRAIT_1 = 1:5
                                                                          , TRAIT_2 = 5:9
                                                                          , GROUP = c("A", "A", "A", "B", "B"))
                                          , mat.species.overlap = data.frame(species = c("a", "c", "d")
                                                                             , raster = "Abund_YEAR_800_C1_STRATA_all.tif"))
                 , "`mat.species.traits` contains some groups with only one species : B", fixed = TRUE)
})
