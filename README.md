
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.bsts (under construction)

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This package provides a tidy R interface to the bsts forecasting
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

Each series generally exhibits an increasing trend with an annual
seasonal pattern that varies proportionally to the level of the series.
At a monthly level, any holiday effects can be modelled using a seasonal
term. A piecewise linear trend is included, as well.

``` r
library(fable.bsts)
#> Loading required package: Rcpp
#> Loading required package: fabletools
fit <- cafe %>% 
  model(
    bsts = BSTS(Turnover ~ season("year") + trend())
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

Note that the annual seasonal pattern does not change very quickly,
although it does differ slightly between years. A very differently
seasonal pattern can be seen for the Northern Territory. We can also
produce forecasts for each of these series over the next two years.

``` r
fc <- fit %>% 
  forecast(h = 24)
```
