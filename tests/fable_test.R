devtools::load_all(".")
library(fabletools)
library(future)
library(fable.prophet)
plan(multisession)

data %>%
  model(
    naive = NAIVE(Count)
    ,arima = ARIMA(Count)
    ,prophet = prophet(Count ~ growth())
    ,bsts_intercept = BSTS(Count ~ intercept(),
                           iterations = 200)
    ,bsts_autoar = BSTS(Count ~ ar("auto"),
                        iterations = 200)
    ,bsts_ar = BSTS(Count ~ ar("specified", lags = 2),
                    iterations = 200)
    ,bsts_level = BSTS(Count ~ level(),
                       iterations = 200)
    ,bsts_local = BSTS(Count ~ trend("local"),
                       iterations = 200)
    ,bsts_semilocal = BSTS(Count ~ trend("semilocal"),
                           iterations = 200)
    ,bsts_student = BSTS(Count ~ trend("student"),
                         iterations = 200)
    ,bsts_int_autoar = BSTS(Count ~ intercept() + ar("auto"),
                            iterations = 200)
    ,bsts_seasonal = BSTS(Count ~ seasonal(period = "1 week"),
                          iterations = 200)
    ,bsts_seas_local = BSTS(Count ~ seasonal("1 week") + trend(),
                            iterations = 200)
    ,bsts_semi_seas = BSTS(Count ~ seasonal("1 week") + trend("semilocal"),
                           iterations = 200)
    ,bsts_trig = BSTS(Count ~ trig(period = "1 week", frequencies = 1),
                      iterations = 200)
  ) %>%
  forecast(h = 50) %>%
  autoplot(data = data[650:nrow(data),]) +
  facet_wrap(~.model)
