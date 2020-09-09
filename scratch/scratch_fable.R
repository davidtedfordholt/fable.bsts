# library(dplyr)
# library(fable)
# library(tsibble)
# library(tsibbledata)
# library(ggplot2)
# source("./R/model.R")
#
# # future::plan(multisession)
#
# .data <- nyc_bikes %>%
#   index_by(day = as.Date(start_time)) %>%
#   # tsibble::index_by(month = tsibble::yearmonth(Time)) %>%
#   group_by_key() %>%
#   summarise(trips = n()) %>%
#   fill_gaps(trips = 0)
#
# plot(.data)
# autoplot(.data) + facet_wrap(.~ bike_id)
#
# iterations <- 100
# horizon <- 100
#
# .data %>%
#   as_tibble() %>%
#   group_by(bike_id) %>%
#   summarise(days = n()) %>%
#   arrange(days)
#
# mbl <-
#   .data %>%
#   # filter(bike_id != 33074) %>%
#   # filter(bike_id != 33074 && bike_id != 31735 && bike_id != 31681) %>%
#   model(
#     naive = NAIVE(trips)
#     # ,snaive = SNAIVE(trips)
#     # ,snaive_drift = SNAIVE(trips ~ drift())
#     # ,arima = ARIMA(trips)
#     # ,bsts_intercept = BSTS(trips ~ intercept(), iterations = iterations)
#     # ,bsts_autoar = BSTS(trips ~ ar("auto"), iterations = iterations)
#     # ,bsts_ar = BSTS(trips ~ ar("specified", lags = 2), iterations = iterations)
#     # ,bsts_level = BSTS(trips ~ level(), iterations = iterations)
#     # ,bsts_local = BSTS(trips ~ trend("local"), iterations = iterations)
#     # ,bsts_semilocal = BSTS(trips ~ trend("semilocal"), iterations = iterations)
#     # ,bsts_student = BSTS(trips ~ trend("student"), iterations = iterations)
#     # ,bsts_int_autoar = BSTS(trips ~ intercept() + ar("auto"), iterations = iterations)
#     # ,bsts_seasonal = BSTS(trips ~ level() + seasonal(period = "1 week"), iterations = iterations)
#     # ,bsts_seas_local = BSTS(trips ~ seasonal("1 week") + trend(), iterations = iterations)
#     # ,bsts_semi_seas = BSTS(trips ~ seasonal("1 week") + trend("semilocal"), iterations = iterations)
#     # ,bsts_trig = BSTS(trips ~ level() + trig(period = "1 week"), iterations = iterations)
#     ,bsts_cycle = BSTS(trips ~ cycle(), iterations = iterations)
#     # ,bsts_broken = BSTS(trips ~ intercept() + ar() + level() + trend() + seasonal() + trig())
#   )
#
# fbl <-
#   mbl %>%
#   forecast(h = horizon)
#
# fbl %>%
#   autoplot(data = .data) +
#   facet_grid(.model ~ bike_id)
