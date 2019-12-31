
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.bsts (under construction)

[![Travis build
status](https://travis-ci.org/mitchelloharawild/fable.prophet.svg?branch=master)](https://travis-ci.org/mitchelloharawild/fable.prophet)
[![Codecov test
coverage](https://codecov.io/gh/mitchelloharawild/fable.prophet/branch/master/graph/badge.svg)](https://codecov.io/gh/mitchelloharawild/fable.prophet?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This package provides a tidy R interface to the prophet forecasting
procedure using [fable](https://github.com/tidyverts/fable). This
package makes use of the [bsts
package](https://cran.r-project.org/package=bsts) for R.

## Installation

You can install the development version of fable.bsts from
[Github](https://github.com/davidtedfordholt/fable.bsts) with:

``` r
# install.packages("remotes")
remotes::install_github("davidtedfordholt/fable.bsts")
```

## Example

Suppose we wanted to model Australia’s monthly turnover for cafes,
restaurants and catering services. The data is available from the
Australian Bureau of Statistics catalogue 8501.0, and in the
[tsibbledata](https://github.com/tidyverts/tsibbledata) package.

``` r
library(tsibble)
library(dplyr)
cafe <- tsibbledata::aus_retail %>% 
  filter(Industry == "Cafes, restaurants and catering services")
```

<img src="man/figures/README-plot-1.png" width="100%" />

Each series generally exhibits an increasing trend with an annual
seasonal pattern that varies proportionally to the level of the series.
At a monthly level, any holiday effects can be modelled using a seasonal
term. A piecewise linear trend is included by default, and so it is not
included in the model specification below.

``` r
library(fable.bsts)
#> Loading required package: Rcpp
#> Loading required package: fabletools
fit <- cafe %>% 
  model(
    bsts = BSTS(Turnover ~ season("year"))
  )
```

``` r
fit
#> # A mable: 8 x 3
#> # Key:     State, Industry [8]
#>   State                      Industry                              bsts 
#>   <chr>                      <chr>                                 <model> 
#> 1 Australian Capital Territ… Cafes, restaurants and catering serv… <bsts…
#> 2 New South Wales            Cafes, restaurants and catering serv… <bsts…
#> 3 Northern Territory         Cafes, restaurants and catering serv… <bsts…
#> 4 Queensland                 Cafes, restaurants and catering serv… <bsts…
#> 5 South Australia            Cafes, restaurants and catering serv… <bsts…
#> 6 Tasmania                   Cafes, restaurants and catering serv… <bsts…
#> 7 Victoria                   Cafes, restaurants and catering serv… <bsts…
#> 8 Western Australia          Cafes, restaurants and catering serv… <bsts…
```

The above output confirms that this bsts model has been fitted to
each of the time series. Components from this model can be extracted:

``` r
components(fit)
```

<img src="man/figures/README-components-plot-1.png" width="100%" /><img src="man/figures/README-components-plot-2.png" width="100%" />

Note that the annual seasonal pattern does not change very quickly,
although it does differ slightly between years. A very differently
seasonal pattern can be seen for the Northern Territory. We can also
produce forecasts for each of these series over the next two years.

``` r
fc <- fit %>% 
  forecast(h = 24)
```

<img src="man/figures/README-fable-1.png" width="100%" />
