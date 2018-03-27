
rm(list=ls())
setwd("/home/gueguen/Documents/_TUTOS/3_R/_PACKAGES/")
# setwd("/Users/gueguen/Documents/PACKAGES/")

library(devtools)
library(tools)
library(roxygen2)
library(usethis)
# devtools::install_github("hadley/pkgdown")
library(pkgdown)

#####################################################################
## INFORMATION / PARAMETERS

package_name = "RFate"
package_version = "0.0.0.9000"

#####################################################################

load("RFate/data/MontBlanc.rda")
devtools::use_data(MontBlanc, pkg = package_name, overwrite = TRUE)

setwd("RFate/")
usethis::use_testthat()
usethis::use_test(name ="PRE_functions")

usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()

#####################################################################
## RUN Roxygen ------------------------------------------------------

roxygenize(
  package.dir = package_name
  , clean = TRUE)

## BUILD THE PACKAGE ------------------------------------------------

Rcmd(args = paste0("build ",package_name)) ## or with a shell : R CMD build MyPackage

## CHECK THE PACKAGE ------------------------------------------------

# Rcmd(args = paste0("check ",package_name)) ## or with a shell : R CMD check MyPackage
devtools::check(pkg = package_name)

# INSTALL THE PACKAGE ----------------------------------------------

install.packages(pkgs = paste0(package_name, "_", package_version, ".tar.gz"), repos = NULL, type = "source")
eval(parse(text = paste0("library(package = ", package_name, ")")))

