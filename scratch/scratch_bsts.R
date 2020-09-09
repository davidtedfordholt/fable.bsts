# library(dplyr)
# library(lubridate)
# library(tsibble)
# library(bsts)
# library(fable)
# library(tsibbledata)
#
# .data <- nyc_bikes %>%
#   index_by(day = as.Date(start_time)) %>%
#   # tsibble::index_by(month = tsibble::yearmonth(Time)) %>%
#   # group_by_key() %>%
#   summarise(trips = n()) %>%
#   fill_gaps(trips = 0)
#
# iterations <- 100
# horizon <- 100
#
# # .data <- .data %>% filter(bike_id == 26301)
#
# zoo_data <- zoo::zoo(
#   x = dplyr::pull(.data[tsibble::measured_vars(.data)], 1),
#   order.by = dplyr::pull(.data[tsibble::index_var(.data)], 1)
# )
#
#
# # Initialize state specification
# state <- list()
#
# # INTERCEPT --------------------------------------------------------------------------------------
#
# if ("intercept" %in% names(specials)) {
#
#   # check intercept validity
#   if (length(specials$intercept) > 1) {
#     rlang::abort("The state should include at most one stationary component.")
#   }
#   if ("level" %in% names(specials) || "trend" %in% names(specials)) {
#     rlang::abort("Models with levels, linear trends or regressions don't need intercepts.")
#   }
#
#   intercept <- specials$intercept[[1]]
#
#   state <- bsts::AddStaticIntercept(
#     state.specification = state,
#     y = zoo_data
#   )
# }
#
# # AR and AUTOAR ----------------------------------------------------------------------------------
#
# if ("ar" %in% names(specials)) {
#
#   # check AR validity
#   if (length(specials$ar) > 1 || "level" %in% names(specials) || "trend" %in% names(specials)) {
#     rlang::abort("The state should include at most one non-stationary trend model (ar, trend, level).")
#   }
#
#   ar <- specials$ar[[1]]
#
#   if (ar$type == "auto") {
#     state <- bsts::AddAutoAr(
#       state.specification = state,
#       y = zoo_data,
#       lags = ar$lags
#     )
#   } else if (ar$type == "specified") {
#     state <- bsts::AddAr(
#       state.specification = state,
#       y = zoo_data,
#       lags = ar$lags
#     )
#   }
# }
#
# # LEVEL ------------------------------------------------------------------------------------------
#
# if ("level" %in% names(specials)) {
#
#   # check level validity
#   if (length(specials$level) > 1 || "trend" %in% names(specials)) {
#     rlang::abort("The state should include at most one non-stationary trend model (ar, trend, level).")
#   }
#
#   level <- specials$level[[1]]
#
#   state <- bsts::AddLocalLevel(
#     state.specification = state,
#     y = zoo_data
#   )
#
# }
#
# # TREND ------------------------------------------------------------------------------------------
#
# if ("trend" %in% names(specials)) {
#
#   # check for trend validity
#   if (length(specials$trend) > 1) {
#     rlang::abort("The state should include at most one non-stationary trend model (ar, trend, level).")
#   }
#
#   trend <- specials$trend[[1]]
#
#   if (trend$type == "local") {
#     state <- bsts::AddLocalLinearTrend(
#       state.specification = state,
#       y = zoo_data
#     )
#   } else if (trend$type == "semilocal") {
#     state <- bsts::AddSemilocalLinearTrend(
#       state.specification = state,
#       y = zoo_data
#     )
#   } else if (trend$type == "studentlocal") {
#     state <- bsts::AddStudentLocalLinearTrend(
#       state.specification = state,
#       y = zoo_data,
#       save.weights = FALSE
#     )
#   }
# }
#
# # SEASONAL ---------------------------------------------------------------------------------------
#
# if ("seasonal" %in% names(specials)) {
#
#   # # check seasonal validity
#   # # NOT well-implemented, but I'm not sure how specials$season is structured
#   # if (length(specials$season) > 1) {
#   #   periods <- c()
#   #   for (season in specials$season) {
#   #     periods <- c(periods, season$period)
#   #   }
#   #   if (any(duplicated(periods))) {
#   #     rlang::rlang::abort("No more than one seasonal model can be specified for a single period.")
#   #   }
#   # }
#
#   for (seasonal in specials$seasonal) {
#
#     # check validity
#     if (!"period" %in% names(seasonal)) {
#       rlang::abort("period must be defined for regression seasonality.")
#     }
#
#     state <- bsts::AddSeasonal(
#       state.specification = state,
#       y = zoo_data,
#       nseasons = seasonal$period
#     )
#   }
# }
#
#
# # TRIG -------------------------------------------------------------------------------------------
#
# if ("trig" %in% names(specials)) {
#
#   for (trig in specials$trig) {
#
#     # check trig validity
#     if (!"period" %in% names(trig) || !"frequencies" %in% names(trig)) {
#       rlang::abort("period and frequencies must be defined for trig seasonality.")
#     }
#     if (!trig$period > 0 || any(!trig$frequencies > 0)) {
#       rlang::abort("period and frequencies must be positive for trig seasonality.")
#     }
#
#     state <- bsts::AddTrig(
#       state.specification = state,
#       y = zoo_data,
#       period = trig$period,
#       frequencies = trig$frequencies
#     )
#   }
# }
#
#
# # CYCLE ------------------------------------------------------------------------------------------
#
# if ("cycle" %in% names(specials)){
#
#   # check for cycle validity
#   if (length(specials$cycle) > 1) {
#     rlang::abort("Only one Monthly-Annual Cycle can be specified.")
#   }
#   if (frequency(.data) != 7) {
#     rlang::abort("Monthly-Annual Cycle (cycle) can only be used with daily data.")
#   }
#
#   state <- bsts::AddMonthlyAnnualCycle(
#     state.specification = state
#     ,y = zoo_data
#     # ,date.of.first.observation = min(dplyr::pull(.data[tsibble::index_var(.data)[1]], 1))
#   )
#
# }
#
#
# # HOLIDAYS ---------------------------------------------------------------------------------------
#
# # if ("holiday" %in% names(specials)) {
# #   holiday <- specials$holiday[[1]]
# #   for (holiday in specials$holiday) {
# #     holiday_type <- trimws(tolower(holiday$type))
# #
# #     if (is_missing(holiday_type) || holiday_type %in% c("reg", "regression")) {
# #       state <- bsts::AddRegressionHoliday(
# #         state.specification = state,
# #         y = vec_data,
# #         holiday.list = holiday$holidays_list,
# #         time0 = holiday$first_observation,
# #         prior = holiday$prior)
# #     } else if (holiday_type %in% c("randomwalk", "rw")) {
# #       state <- bsts::AddRandomWalkHoliday(
# #         state.specification = state,
# #         y = vec_data,
# #         holiday = holiday$holidays_list,
# #         time0 = holiday$first_observation,
# #         sigma.prior = holiday$sigma_prior,
# #         initial.state.prior = holiday$initial_state_prior
# #       )
# #     } else if (holiday_type %in% c("hierarchical", "hierarchicalregression", "hr", "hreg")) {
# #       state <- bsts::AddHierarchicalRegressionHoliday(
# #         state.specification = state,
# #         y = vec_data,
# #         holiday.list = holiday$holidays_list,
# #         coefficient.mean.prior = holiday$coefficient_mean_prior,
# #         coefficient.variance.prior = holiday$coefficient_variance_prior,
# #         time0 = holiday$first_observation
# #       )
# #     }
# #   }
# # }
#
#
# # EXOGENOUS REGRESSORS ---------------------------------------------------------------------------
#
# # if ("xreg" %in% names(specials)) {
# #   xreg_data <-
# #     if (nrow(xreg_data) != length(vec_data)) {
# #       rlang::abort("The number of observations in ")
# #     }
# #
# #   for (regressor in specials$xreg) {
# #     for (nm in colnames(regressor$xreg)) {
# #       model_data[nm] <- regressor$xreg[,nm]
# #
# #       if (nrow(xreg_data) != length(vec_data)) {
# #         rlang::abort("The number of observations in ")
# #       }
# #
# #
# #       state <- bsts::AddDynamicRegression(
# #         state, name = nm, )
# #     }
# #   }
# # }
#
#
#
# # TRAIN MODEL ------------------------------------------------------------------------------------
#
# # bsts_quietly <- purrr::quietly(bsts::bsts)
# #
# # mdl <- bsts_quietly(
# #   vec_data,
# #   state.specification = state,
# #   niter = iterations
# # )
#
# mdl <- bsts::bsts(
#   zoo_data,
#   state.specification = state,
#   niter = iterations,
#   ping = 0
# )
#
#
#
# plot(mdl, "components")
#
# pred <- predict(mdl, horizon = 50)
# sim <- split(pred$distribution, col(pred$distribution))
#
# fabletools::construct_fc(
#   point = pred$mean,
#   sd = apply(pred$distribution, 2, stats::sd),
#   dist = fabletools::dist_sim(sim)
# )
#
# cmp <- cbind.data.frame(
#   .data, t(colMeans(mdl$state.contributions))) %>%
#   tsibble::as_tsibble()
#
# cmp$.resid <-
#   c(NA,
#     colMeans(mdl$one.step.prediction.errors)[2:nrow(cmp)])
# mv <- tsibble::measured_vars(cmp)
# fabletools::as_dable(cmp, resp = !!sym(mv[1]), method = "bsts",
#   aliases = set_names(
#     list(expr(!!sym("trend") * (1 + !!sym("multiplicative_terms")) + !!sym("additive_terms") + !!sym(".resid"))),
#       mv[1]
#   )
# )
#
# # plot(mdl)
#
# # plot(pred)
