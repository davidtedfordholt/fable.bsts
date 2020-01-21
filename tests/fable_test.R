devtools::load_all(".")
library(tidyverse)
library(fable)
library(fabletools)
library(future)
plan(multisession)

source("data-raw/lax_passengers.R")
data <- lax_passengers %>%
  fill_gaps()

data <- tsibbledata::vic_elec %>%
  as_tibble() %>%
  group_by(Date) %>%
  summarize(Demand = mean(Demand),
            Temperature = mean(Temperature),
            Holiday = all(Holiday)) %>%
  tsibble::as_tsibble()


data %>%
  fabletools::model(
    naive = fable::NAIVE(Demand)
    ,arima = fable::ARIMA(Demand)
    # ,bsts_intercept = BSTS(passengers ~ intercept(),
    #                        iterations = 200)
    # ,bsts_autoar = BSTS(passengers ~ ar("auto"),
    #                    iterations = 200)
    # ,bsts_ar = BSTS(passengers ~ ar("specified", lags = 2),
    #                 iterations = 200)
    # ,bsts_level = BSTS(passengers ~ level(),
    #                    iterations = 200)
    # ,bsts_local = BSTS(passengers ~ trend("local"),
    #                    iterations = 200)
    # ,bsts_semilocal = BSTS(passengers ~ trend("semilocal"),
    #                        iterations = 200)
    # ,bsts_student = BSTS(passengers ~ trend("student"),
    #                      iterations = 200)
    # ,bsts_int_autoar = BSTS(passengers ~ intercept() + ar("auto"),
    #                         iterations = 200)
    # ,bsts_seasonal = BSTS(passengers ~ seasonal(period = "1 week"),
    #                       iterations = 200)
    # ,bsts_seas_local = BSTS(passengers ~ seasonal("1 week") + trend(),
    #                         iterations = 200)
    # ,bsts_semi_seas = BSTS(passengers ~ seasonal("1 week") + trend("semilocal"),
    #                        iterations = 200)
     ,bsts_trig = BSTS(Demand ~ level() + trig(period = "1 week", frequencies = 1),
                       iterations = 200)
     ,bsts_cycle = BSTS(Demand ~ level() + cycle(),
                        iterations = 200)
  ) %>%
  fabletools::forecast(h = 100) %>%
  autoplot(data = data) +
  facet_grid(rows = vars(.model))
