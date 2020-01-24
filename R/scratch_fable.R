# devtools::load_all(".")
library(tidyverse)
library(fable)
source("./R/model.R")

# future::plan(multisession)

tictoc::tic()
data <- tsibbledata::nyc_bikes %>%
  tsibble::index_by(day = as.Date(start_time)) %>%
  # tsibble::index_by(month = tsibble::yearmonth(Time)) %>%
  tsibble::group_by_key() %>%
  dplyr::summarise(trips = n()) %>%
  tsibble::fill_gaps(trips = 0)

data %>%
  fabletools::model(
    naive = fable::NAIVE(trips)
    ,snaive = fable::SNAIVE(trips)
    ,arima = fable::ARIMA(trips)
    # ,bsts_intercept = BSTS(trips ~ intercept(), iterations = 500)
    # ,bsts_autoar = BSTS(trips ~ ar("auto"), iterations = 500)
    # ,bsts_ar = BSTS(trips ~ ar("specified", lags = 2), iterations = 500)
    # ,bsts_level = BSTS(trips ~ level(), iterations = 500)
    # ,bsts_local = BSTS(trips ~ trend("local"), iterations = 500)
    # ,bsts_semilocal = BSTS(trips ~ trend("semilocal"), iterations = 500)
    # ,bsts_student = BSTS(trips ~ trend("student"), iterations = 500)
    # ,bsts_int_autoar = BSTS(trips ~ intercept() + ar("auto"), iterations = 500)
    # ,bsts_seasonal = BSTS(trips ~ level() + seasonal(period = "1 week"), iterations = 500)
    # ,bsts_seas_local = BSTS(trips ~ seasonal("1 week") + trend(), iterations = 500)
    # ,bsts_semi_seas = BSTS(trips ~ seasonal("1 week") + trend("semilocal"), iterations = 500)
    # ,bsts_trig = BSTS(trips ~ level() + trig(period = "1 week"), iterations = 500)
    # ,bsts_cycle = BSTS(trips ~ level() + cycle(), iterations = 500)
    # ,bsts_broken = BSTS(trips ~ intercept() + ar() + level() + trend() + seasonal() + trig())
  ) %>%
  fabletools::forecast(h = 100) %>%
  fabletools::autoplot(data = data) +
  ggplot2::facet_grid(~ .model + bike_id)
tictoc::toc()
