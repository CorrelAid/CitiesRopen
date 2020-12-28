### loading packages

pkgs <- c("devtools", "roxygen2", "usethis", "testthat", "available")
install.packages(pkgs)

library(devtools)
library(roxygen2)
library(usethis)
library(testthat)
library(available)

available("citiesRopen", browse = F)

create_tidy_package(".")
