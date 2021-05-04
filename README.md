CitiesRopen
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# CitiesRopen <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/CitiesRopen)](https://CRAN.R-project.org/package=CitiesRopen)
<!-- badges: end -->

The goal of CitiesRopen is to provide an easy access to the Open Data
Portal of the City of Konstanz \[<https://offenedaten-konstanz.de/>\]

## Status

The package is under continuous development and will be extended with
additional features in the future.

## Installation

You can install the current version of CitiesRopen from
[Github](https://github.com/PhilippMartinBosch/CitiesRopen) with:

``` r
devtools: install_github(“PhilippMartinBosch/CitiesRopen”)
```

## Example

The package contains two major functions. You can use the Pipe-Operator
’ %\>% ’ from margritter to combine them.

``` r
show_data() %>% 
  get_data() 
```

…
