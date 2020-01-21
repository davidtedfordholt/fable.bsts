devtools::load_all(".")
library(tidyverse)
library(fable)
library(fabletools)
library(future)
plan(sequential)


data <- tsibbledata::vic_elec %>%
  tsibble::index_by(day = as.Date(Time)) %>%
  dplyr::summarise(Demand = sum(Demand, na.rm = TRUE))

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
<<<<<<< HEAD
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
    ,bsts_seasonal = BSTS(Demand ~ seasonal(period = "1 week"),
                          iterations = 500)
    ,bsts_seas_local = BSTS(Demand ~ seasonal("1 week") + trend(),
                            iterations = 500)
    ,bsts_semi_seas = BSTS(Demand ~ seasonal("1 week") + trend("semilocal"),
                           iterations = 500)
    ,bsts_trig = BSTS(Demand ~ trig(period = "1 week", frequencies = 1),
                      iterations = 500)
    ,bsts_cycle = BSTS(Demand ~ cycle(),
                       iterations = 500)
=======
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
>>>>>>> 0b13595d0bb4d67b076d6acc2a272aa5b964c9cb
  ) %>%
  fabletools::forecast(h = 100) %>%
  fabletools::autoplot(data = data %>%
                         filter(day >= as.Date("2014-10-01"))) +
  ggplot2::facet_wrap(~ .model)
