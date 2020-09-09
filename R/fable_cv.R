# CLEAN ENVIRONMENT ================================================================================

rm(list = ls(all.names = TRUE))


# PACKAGES =========================================================

# library(tictoc)
# tictoc::tic("total time")
# tictoc::tic("loading packages")
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fable.prophet)
library(future)
library(future.apply)
future::plan("multisession")
# tictoc::toc()


# FUNCTIONS ========================================================

create_cutoff_dates <-
  function(data, n_cv, horizon = 1) {
    max_date <- max(pull(data[index_var(data)]))
    cv <- tibble::tibble(
      cutoff_date = seq.Date(from = max_date - n_cv,
                             to = max_date - horizon, by = 1))
    as.list(environment())
  }

create_forecasts <-
  function(cutoff_date, data, horizon) {
    data %>%
      dplyr::filter(Date <= cutoff_date) %>%
      fabletools::model(
        snaive = fable::SNAIVE(Demand ~ lag("week"))
        ,ets = fable::ETS(Demand)
      ) %>%
      fabletools::forecast(h = horizon) %>%       # Forecast
      dplyr::mutate(cutoff = cutoff_date)
  }

create_forecasts_for_all_cutoffs <-
  function(object) {
    object$cv <-
      object$cv %>%
      mutate(
        forecasts = furrr::future_map(
          cutoff_date,
          create_forecasts,
          data = object$data,
          horizon = object$horizon,
          .progress = TRUE))
    object
  }

pull_forecasts_from_df <-
  function(object) {
    object$cv %>%
      dplyr::pull(forecasts) %>%
      purrr::map_dfr(`[`)
  }


# DATA =============================================================

data <- tsibbledata::vic_elec %>%
  tsibble::index_by(Date) %>%
  dplyr::summarise(Demand = sum(Demand))


# MODELING =========================================================

# tictoc::tic("Modeling")
object <-
  data %>%
  create_cutoff_dates(n_cv = 62, horizon = 14)

forecasts <-
  object %>%
  create_forecasts_for_all_cutoffs()


# tictoc::toc()
# tictoc::toc()
