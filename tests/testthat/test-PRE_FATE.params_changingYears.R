library(RFate)
context("PRE_FATE.params_changingYears() function")

## INPUTS
test_that("PRE_FATE.params_changingYears gives error with missing data", {
  expect_error(PRE_FATE.params_changingYears()
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(NA)
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(NULL)
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
})


## INPUTS
test_that("PRE_FATE.params_changingYears gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_changingYears(1)
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears("a")
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(factor(1))
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  expect_error(PRE_FATE.params_changingYears(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
})


## INPUTS
test_that("PRE_FATE.params_changingYears gives error with wrong data : type.changing and mat.changing", {
  PRE_FATE.skeletonDirectory()
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation")
               , "`type.changing` must be either `MASK`, `HS` or `DIST`", fixed = T)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = NULL)
               , "`type.changing` must be either `MASK`, `HS` or `DIST`", fixed = T)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = NA)
               , "`type.changing` must be either `MASK`, `HS` or `DIST`", fixed = T)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "dist")
               , "`type.changing` must be either `MASK`, `HS` or `DIST`", fixed = T)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST")
               , "`mat.changing` must be a data.frame")
  
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = NA)
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = NULL)
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = "")
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = 1)
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = factor(1))
               , "`mat.changing` must be a data.frame")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = matrix(1))
               , "`mat.changing` must be a data.frame")
  
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame())
               , "`mat.changing` does not have the appropriate number of rows (>0) or columns (year, order, file.name)", fixed = T)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(1))
               , "`mat.changing` does not have the appropriate number of rows (>0) or columns (year, order, file.name)", fixed = T)
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(1,2,3))
               , "Column names of `mat.changing` must be `year`, `order` and `file.name`")
  
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = NA, order = 1, file.name = "hop"))
               , "Columns `year` and `order` of `mat.changing` must contain numeric values")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = "a", order = 1, file.name = "hop"))
               , "Columns `year` and `order` of `mat.changing` must contain numeric values")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = factor(1), order = 1, file.name = "hop"))
               , "Columns `year` and `order` of `mat.changing` must contain numeric values")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = c(10,NA), order = 1, file.name = "hop"))
               , "Columns `year`, `order` and `file.name` of `mat.changing` must not contain NA values")
  
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = NA, file.name = "hop"))
               , "Columns `year` and `order` of `mat.changing` must contain numeric values")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = "a", file.name = "hop"))
               , "Columns `year` and `order` of `mat.changing` must contain numeric values")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = factor(1), file.name = "hop"))
               , "Columns `year` and `order` of `mat.changing` must contain numeric values")
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = c(1,NA), file.name = "hop"))
               , "Columns `year`, `order` and `file.name` of `mat.changing` must not contain NA values")
  
  
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = 10, order = 1, file.name = c("",NA)))
               , "Columns `year`, `order` and `file.name` of `mat.changing` must not contain NA values")
  
  expect_error(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                             , mat.changing = data.frame(year = c(10,10,50), order = c(1,2,1), file.name = c(1,2,3)))
               , "Columns `year` and `order` are not balanced\n All combinations must be represented")
})


## OUTPUTS
test_that("PRE_FATE.params_changingYears gives correct output", {
  if (dir.exists("FATE_simulation")) system("rm -r FATE_simulation/")
  PRE_FATE.skeletonDirectory()
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B")))
                 , "The parameter file FATE_simulation/DATA/SCENARIO/DIST_changing_times.txt has been successfully created !")
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B")))
                 , "The parameter file FATE_simulation/DATA/SCENARIO/DIST_changing_masks_t10.txt has been successfully created !")
  expect_warning(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B")))
                 , "already exists. It will be replaced.")
  
  
  expect_warning(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B"))
                                               , opt.folder.name = NA)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  expect_warning(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B"))
                                               , opt.folder.name = 1)
                 , "As `opt.folder.name` does not contain character value, it will be ignored")
  
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B"))
                                               , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/SCENARIO/scen1/DIST_changing_times.txt has been successfully created !")
  expect_message(PRE_FATE.params_changingYears(name.simulation = "FATE_simulation", type.changing = "DIST"
                                               , mat.changing = data.frame(year = c(10,10), order = c(1,2), file.name = c("A","B"))
                                               , opt.folder.name = "scen1")
                 , "The parameter file FATE_simulation/DATA/SCENARIO/scen1/DIST_changing_masks_t10.txt has been successfully created !")
})
