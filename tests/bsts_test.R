library(tidyverse)
library(lubridate)
library(tsibble)
library(bsts)
library(fable)

data <- tsibbledata::vic_elec %>%
  tsibble::index_by(day = as.Date(Time)) %>%
  # tsibble::index_by(month = tsibble::yearmonth(Time)) %>%
  dplyr::summarise(Demand = sum(Demand, na.rm = TRUE))

vec_data <- dplyr::pull(data[tsibble::measured_vars(data)], 1)

state <- list()

# Intercept
state <- AddStaticIntercept(state, y = vec_data)

# AR
# state <- AddAr(state, y = vec_data, lags = 1)
# state <- AddAutoAr(state, y = vec_data)

# Level
# state <- AddLocalLevel(state, y = vec_data)

# Trend
# state <- AddLocalLinearTrend(state, y = vec_data)
# state <- AddSemilocalLinearTrend(state, y = vec_data)
# state <- AddStudentLocalLinearTrend(state, y = vec_data)

# Seasonality
# state <- AddSeasonal(state, y = vec_data, nseasons = 7)
# state <- AddTrig(state, y = vec_data,
#                  period = 365, frequencies = c(1, 52, 12))
# state <- AddMonthlyAnnualCycle(state, y = vec_data)

# External Regressors
# state <- AddDynamicRegression(state, model_data)

# Holidays
# state <- AddHierarchicalRegressionHoliday(state)
# state <- AddRegressionHoliday(state)
# state <- AddRandomWalkHoliday(state)

# Prior
# if external regressors
# prior <- SpikeSlabPrior(); prior <- SpikeSlabArPrior()
# if not
# prior <- SdPrior(sigma.guess = sd(vec_data))

# Family
# family <- c("gaussian", "logit", "poisson", "student")

mdl <- bsts(
  vec_data,
  state.specification = state,
  # prior = prior,
  niter = 500)

plot(mdl, "components")

pred <- predict(mdl, horizon = 50)
sim <- split(pred$distribution, col(pred$distribution))

fabletools::construct_fc(
  point = pred$mean,
  sd = apply(pred$distribution, 2, stats::sd),
  dist = fabletools::dist_sim(sim)
)

cmp <- cbind.data.frame(
  data, t(colMeans(mdl$state.contributions))) %>%
  tsibble::as_tsibble()

cmp$.resid <-
  c(NA,
    colMeans(mdl$one.step.prediction.errors)[2:nrow(cmp)])
mv <- tsibble::measured_vars(cmp)
fabletools::as_dable(cmp, resp = !!sym(mv[1]), method = "bsts",
  aliases = set_names(
    list(expr(!!sym("trend") * (1 + !!sym("multiplicative_terms")) + !!sym("additive_terms") + !!sym(".resid"))),
      mv[1]
  )
)

# plot(mdl)

# plot(fits)
