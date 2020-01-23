
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.bsts

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This package provides a tidy R interface to the bsts forecasting
procedure using [fable](https://github.com/tidyverts/fable). This
package makes use of the [bsts
package](https://cran.r-project.org/package=bsts) for R.

*While the `BSTS()` function works in the `fable::model()` framework,
not all functionality from the package has been implemented.*

## Completed

  - Stationary and non-stationary trend models
  - Regression, trigonometric and monthly-annual cycle seasonal models

## In Progress

  - Holiday models
  - Exogenous regressors
  - User-specified priors

## Use

``` r
# library(tidyverse)
# 
# source("data-raw/lax_passengers.R")
# data <- lax_passengers %>%
#   mutate(date = yearmonth(date))
#   
# data %>%
#   fabletools::model(
#     bsts = BSTS(passengers ~ intercept() + ar() + seasonal("1 day"))
#   ) %>%
#   fabletools::forecast(h = 48) %>%
#   autoplot()
```

### Specials implemented:

  - `intercept()` - static intercept models
  - `ar()` - autoregressive models
  - `level()` - local and shared level models
  - `trend()` - local linear, semilocal linear and Student local linear
    models
  - `seasonal()` - seasonal regression models
  - `trig()` - trigonometric seasonality models
