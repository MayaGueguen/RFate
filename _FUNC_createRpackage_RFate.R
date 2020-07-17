
rm(list=ls())
setwd("/home/gueguema/Documents/_TUTOS/3_R/_PACKAGES/")
# setwd("/Users/gueguen/Documents/PACKAGES/")

library(devtools)
library(tools)
library(roxygen2)
library(usethis)
library(spelling)
# devtools::install_github("hadley/pkgdown")
library(pkgdown)
library(raster)


#####################################################################
## INFORMATION / PARAMETERS

package_name = "RFate"
package_version = "0.2.0.9000"

#####################################################################

setwd("RFate/")

tools::checkRdaFiles("data/")

# load("data/MontBlanc.rda")
# usethis::use_data(MontBlanc, overwrite = TRUE)
# load("data_supplements/FATE_Bauges")
# usethis::use_data(FATE_Bauges, overwrite = TRUE)
# load("data_supplements/PNE.PFG.RData")

# source("data-raw/PNE_PFG.R")
# source("data-raw/PNE_PARAM.R")
# source("data-raw/PNE_RESULTS.R")

usethis::use_testthat()
usethis::use_test(name ="PRE_functions")

usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()

spelling::spell_check_setup()

usethis::use_pkgdown()

# devtools::use_build_ignore("_FUNC_createRpackage_RFate.R")
# devtools::use_build_ignore("R/examples.PRE_FATE1.R")
# devtools::use_build_ignore("R/examples.PRE_FATE2.R")
# devtools::use_build_ignore("SHINY.PRE_FATE.params.R")
setwd("./../")

setwd("RFate/")
pkgdown:::build_site(lazy = TRUE)
# pkgdown::template_navbar()

# pkgdown:::init_site()
# pkgdown:::build_home()
pkgdown:::build_articles()
# system(command = "mv docs/articles docs/vignettes")
# pkgdown:::init_site()
pkgdown:::build_home()
file.copy("vignettes/pictures/SCHEMA_succession1.jpg", "docs/articles/pictures/SCHEMA_succession1.jpg")
file.copy("vignettes/pictures/SCHEMA_FATE_WORKFLOW.png", "docs/articles/pictures/SCHEMA_FATE_WORKFLOW.png")
pkgdown:::build_reference(examples = FALSE)
# pkgdown:::build_tutorials()
# pkgdown:::build_news()
setwd("./../")

#####################################################################
## RUN Roxygen ------------------------------------------------------

roxygenize(
  package.dir = package_name
  , clean = TRUE)

setwd("./../")
roxygenize(
  package.dir = package_name
  , clean = TRUE)
setwd("RFate/")
pkgdown:::build_reference(examples = FALSE)



## BUILD THE PACKAGE ------------------------------------------------

Rcmd(args = paste0("build ", package_name)) ## or with a shell : R CMD build MyPackage

## CHECK THE PACKAGE ------------------------------------------------

# Rcmd(args = paste0("check ",package_name)) ## or with a shell : R CMD check MyPackage
# devtools::check(pkg = package_name, document = FALSE)
devtools::check(pkg = package_name)
# testthat::auto_test(code_path = "RFate/R/", test_path = "RFate/tests/testthat/")
testthat::auto_test(code_path = "AA_R/", test_path = "AA_testthat/")

## INSTALL THE PACKAGE ----------------------------------------------

install.packages(pkgs = paste0(package_name, "_", package_version, ".tar.gz"), repos = NULL, type = "source")
system(paste0("scp ", paste0(package_name, "_", package_version, ".tar.gz"), " ", package_name, "/docs/archive/"))
eval(parse(text = paste0("library(package = ", package_name, ")")))

