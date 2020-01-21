# devtools::load_all(".")
library(tidyverse)
library(fable)
library(fabletools)
library(future)

plan(multisession)

tictoc::tic()
data <- tsibbledata::vic_elec %>%
  tsibble::index_by(day = as.Date(Time)) %>%
  # tsibble::index_by(month = tsibble::yearmonth(Time)) %>%
  dplyr::summarise(Demand = sum(Demand, na.rm = TRUE))

data %>%
  fabletools::model(
    naive = fable::NAIVE(Demand)
    ,snaive = fable::SNAIVE(Demand)
    ,arima = fable::ARIMA(Demand)
    ,bsts_intercept = BSTS(Demand ~ intercept(),
                           iterations = 500)
    ,bsts_autoar = BSTS(Demand ~ ar("auto"),
                        iterations = 500)
    ,bsts_ar = BSTS(Demand ~ ar("specified", lags = 2),
                    iterations = 500)
    ,bsts_level = BSTS(Demand ~ level(),
                       iterations = 500)
    ,bsts_local = BSTS(Demand ~ trend("local"),
                       iterations = 500)
    ,bsts_semilocal = BSTS(Demand ~ trend("semilocal"),
                           iterations = 500)
    ,bsts_student = BSTS(Demand ~ trend("student"),
                         iterations = 500)
    ,bsts_int_autoar = BSTS(Demand ~ intercept() + ar("auto"),
                            iterations = 500)
    ,bsts_seasonal = BSTS(Demand ~ level() + seasonal(period = "1 week"),
                          iterations = 500)
    ,bsts_seas_local = BSTS(Demand ~ seasonal("1 week") + trend(),
                            iterations = 500)
    ,bsts_semi_seas = BSTS(Demand ~ seasonal("1 week") + trend("semilocal"),
                           iterations = 500)
    ,bsts_trig = BSTS(Demand ~ level() + trig(period = "1 week", frequencies = 1),
                      iterations = 500)
    ,bsts_cycle = BSTS(Demand ~ level() + cycle(),
                       iterations = 500)
  ) %>%
  fabletools::forecast(h = 100) %>%
  fabletools::autoplot(data = data) +
  ggplot2::facet_wrap(~ .model)
tictoc::toc()
