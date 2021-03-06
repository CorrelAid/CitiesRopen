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

This is a basic example which shows you how to use the functions in the
CitiesRopen Package:

``` r
show_data() %>% 
  get_data()
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
