CitiesRopen
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/CitiesRopen)](https://CRAN.R-project.org/package=CitiesRopen)
<!-- badges: end -->

The goal of CitiesRopen is to provide an easy access to the Open Data
Portal of the City of Konstanz \[<https://offenedaten-konstanz.de/>\]

## Installation

You can install the released version of CitiesRopen from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("CitiesRopen")
```

## Example

The package contains two major functions. You can use the Pipe-Operator
’ %\>% ’ from margritter to combine them.

``` r
show_data() %>% 
  get_data()
```

TBA. Those are some arguments to filter the output
