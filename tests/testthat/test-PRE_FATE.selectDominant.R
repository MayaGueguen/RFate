library(RFate)
context("PRE_FATE.selectDominant() function")

## INPUTS
test_that("PRE_FATE.selectDominant gives error with missing values", {
  expect_error(PRE_FATE.selectDominant(), "No data given!\n (missing", fixed = T)
  expect_warning(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = NA))
                 , "Species with NO abundance information can only be selected with the criteria based on number of presences...")
  expect_warning(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = NA))
                 , "NO abundance information in your data. Dominant species selection will only be done with the criteria based on number of presences...")
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : mat.site.species.abund", {
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = matrix(1)), "Wrong type of data!\n `mat.site.species.abund` must be a data.frame")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = c(1, 89, 3)), "Wrong type of data!\n `mat.site.species.abund` must be a data.frame")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame("1")),  "does not have the appropriate number of rows")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame("1","abc")), "does not have the appropriate number of rows", fixed = T)
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(species = "1",sites = "A")), "does not have the appropriate number of rows", fixed = T)
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(species = "1",sites = "A", abundance = 3)), "Wrong type of data!\n Column names of", fixed = T)
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(species = "1",sites = "A", abundance = 3, habitat = "grass")), "Wrong type of data!\n Column names of", fixed = T)
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(species = "1",sites = "A", abund = "a"))
               , "Wrong type of data!\n Column `abund` of `mat.site.species.abund` must constain positive numeric values", fixed = T)
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(species = "1",sites = "A", abund = factor(1)))
               , "Wrong type of data!\n Column `abund` of `mat.site.species.abund` must constain positive numeric values", fixed = T)
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(species = "1",sites = "A", abund = -0.5))
               , "Wrong type of data!\n Column `abund` of `mat.site.species.abund` must constain positive numeric values", fixed = T)
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : sites / species / abund", {
  expect_error(PRE_FATE.selectDominant(species = "1"), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.selectDominant(sites = "A"), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.selectDominant(species = "1", sites = "A"), "No data given!\n (missing", fixed = T)
  expect_error(PRE_FATE.selectDominant(species = "1", sites = "A", abund = "a")
               , "Wrong type of data!\n Column `abund` of `mat.site.species.abund` must constain positive numeric values", fixed = T)
  expect_error(PRE_FATE.selectDominant(species = "1", sites = "A", abund = factor(1))
               , "Wrong type of data!\n Column `abund` of `mat.site.species.abund` must constain positive numeric values", fixed = T)
  expect_error(PRE_FATE.selectDominant(species = "1", sites = "A", abund = -0.5)
               , "Wrong type of data!\n Column `abund` of `mat.site.species.abund` must constain positive numeric values", fixed = T)
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : selectionRule", {
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.quanti = "a"), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.quanti = factor(1)), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.quanti = 1.1), "must be between 0 and 1")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.quanti = -119), "must be between 0 and 1")

  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.min_mean_abund = "a"), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.min_mean_abund = factor(1)), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.min_mean_abund = -119), "must be >= 0")
  
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.min_no_abund_over25 = "a"), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.min_no_abund_over25 = factor(1)), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , selectionRule.min_no_abund_over25 = -119), "must be >= 0")
  
  expect_warning(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A"
                                                                             , species = "1"
                                                                             , abund = 3)
                                         , doHabitatSelection = TRUE)
                 , "Only 1 value indicated in `habitat` variable")
  expect_warning(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A"
                                                                             , species = "1"
                                                                             , abund = 3
                                                                             , habitat = "Landes")
                                         , doHabitatSelection = TRUE)
                 , "Only 1 value indicated in `habitat` variable")
  expect_warning(PRE_FATE.selectDominant(species = "1", sites = "A", abund = c(3, 10)
                                         , doHabitatSelection = TRUE)
                 , "Only 1 value indicated in `habitat` variable")
  expect_warning(PRE_FATE.selectDominant(species = "1", sites = "A", abund = c(3, 10), habitat = "Landes"
                                         , doHabitatSelection = TRUE)
                 , "Only 1 value indicated in `habitat` variable")
  
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_percent_habitat = "a"), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_percent_habitat = factor(1)), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_percent_habitat = 1.1), "must be between 0 and 1")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_percent_habitat = -119), "must be between 0 and 1")
  
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_no_habitat = "a"), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_no_habitat = factor(1)), "must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3)
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_no_habitat = -119), "must be >= 0")
  
  expect_error(PRE_FATE.selectDominant(species = "1", sites = "A", abund = c(3, 10), habitat = "Landes"
                                       , doHabitatSelection = TRUE
                                       , selectionRule.min_no_habitat = -119), "must be >= 0")
})

## OUTPUTS
test_that("PRE_FATE.selectDominant right results", {
  expect_output(str(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3))), "data.frame")
  expect_output(str(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A", species = "1", abund = 3))), "9 variables")
  expect_output(str(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A"
                                                                                , species = "1"
                                                                                , abund = 3
                                                                                , habitat = "Landes")))
                , "data.frame")
  expect_output(str(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = "A"
                                                                                , species = "1"
                                                                                , abund = 3
                                                                                , habitat = c("Landes", "Grassland"))))
                , "data.frame")
  
  expect_output(str(PRE_FATE.selectDominant(mat.site.species.abund = data.frame(sites = c(rep("A", 50), rep("B", 50))
                                                                                , species = sample(1:5, 100, replace = T)
                                                                                , abund = sample(c(rep(1, 20), seq(30, 50)), 100, replace = T)
                                                                                , habitat = c("Landes", "Grassland"))
                                            , doHabitatSelection = TRUE))
                , "data.frame")
  
})
