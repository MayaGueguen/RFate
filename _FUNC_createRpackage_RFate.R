
rm(list=ls())
setwd("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES/")
# setwd("/Users/gueguen/Documents/PACKAGES/")

library(devtools)
library(tools)
library(roxygen2)
library(usethis)
library(spelling)
# devtools::install_github("hadley/pkgdown")
library(pkgdown)


#####################################################################
## INFORMATION / PARAMETERS

package_name = "RFate"
package_version = "0.0.3.9000"

#####################################################################

load("RFate/data/MontBlanc.rda")
devtools::use_data(MontBlanc, pkg = package_name, overwrite = TRUE)

setwd("RFate/")
usethis::use_testthat()
usethis::use_test(name ="PRE_functions")

usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()

spelling::spell_check_setup()

usethis::use_pkgdown()

devtools::use_build_ignore("_FUNC_createRpackage_RFate.R")
devtools::use_build_ignore("R/examples.PRE_FATE1.R")
devtools::use_build_ignore("R/examples.PRE_FATE2.R")
devtools::use_build_ignore("R/SHINY.PRE_FATE.params.R")
setwd("./../")

setwd("RFate/")
# pkgdown:::build_site(lazy = TRUE)
pkgdown::template_navbar()

# pkgdown:::init_site()
pkgdown:::build_home()
# pkgdown:::build_reference(examples = TRUE)
# pkgdown:::build_articles()
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

Rcmd(args = paste0("build ",package_name)) ## or with a shell : R CMD build MyPackage

## CHECK THE PACKAGE ------------------------------------------------

# Rcmd(args = paste0("check ",package_name)) ## or with a shell : R CMD check MyPackage
devtools::check(pkg = package_name, document = FALSE)
# devtools::check(pkg = package_name)

# INSTALL THE PACKAGE ----------------------------------------------

install.packages(pkgs = paste0(package_name, "_", package_version, ".tar.gz"), repos = NULL, type = "source")
system(paste0("scp ",paste0(package_name, "_", package_version, ".tar.gz"), " ", package_name, "/docs/archive/"))
eval(parse(text = paste0("library(package = ", package_name, ")")))

