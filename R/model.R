#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#===================================================================================================
#
# TRAIN MODEL
#
#===================================================================================================


#' @importFrom stats predict
train_bsts <- function(.data, specials, ...) {
  if (length(tsibble::measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by bsts")
  }

  # Prepare data for modelling
  model_data <- as_tibble(.data)[c(expr_text(index(.data)), measured_vars(.data))]
  vec_data <- model_data %>% pull(measured_vars(.data))

  # Initialize state specification
  state <- list()

  #-------------------------------------------------------------------------------------------------
  # INTERCEPT
  #-------------------------------------------------------------------------------------------------

  if ("intercept" %in% names(specials)) {

    # check intercept validity
    if (length(specials$intercept) > 1) {
      abort("BSTS only supports a single intercept argument")
    }
    if ("level" %in% names(specials) || "trend" %in% names(specials)) {
      abort("If the model includes a traditional trend component (e.g. local level, local linear
            trend, etc) then a separate intercept is not needed (and will probably cause trouble,
            as it will be confounded with the initial state of the trend model).")
    }

    intercept <- specials$intercept[[1]]

    state <- AddStaticIntercept(
      state.specification = state,
      y = vec_data
    )
  }

  #-------------------------------------------------------------------------------------------------
  # AR and AUTOAR
  #-------------------------------------------------------------------------------------------------

  # if ("ar" %in% names(specials)) {
  #
  #   # check intercept validity
  #   if (length(specials$ar) > 1) {
  #     abort("BSTS only supports a single AR argument")
  #   }
  #
  #   ar <- specials$ar[[1]]
  #
  #   if (!"lags" %in% names(trend)) {
  #     state <- AddAutoAr(
  #       state.specification = state,
  #       y = vec_data
  #     )
  #   } else if (trend$type == "ar") {
  #     state <- AddAr(
  #       state.specification = state,
  #       y = vec_data,
  #       lags = trend$lags
  #     )
  #   }
  # }

  #-------------------------------------------------------------------------------------------------
  # LEVEL
  #-------------------------------------------------------------------------------------------------

  # if ("level" %in% names(specials)) {
  #   # check for level validity
  #   if (length(specials$level) > 1) {
  #     abort("BSTS only supports a single level argument")
  #   }
  #
  #   level <- specials$level[[1]]
  #
  #   if (level$type == "locallevel") {
  #     state <- AddLocalLevel(
  #       state.specification = state,
  #       y = vec_data
  #     )
  #   } else if (level$type == "sharedlevel") {
  #     state <- AddSharedLocalLevel(
  #       state.specification = state,
  #       y = vec_data
  #     )
  #   }
  # }

  #-------------------------------------------------------------------------------------------------
  # TREND
  #-------------------------------------------------------------------------------------------------

  # if ("trend" %in% names(specials)) {
  #   # check for trend validity
  #   if (length(specials$trend) > 1) {
  #     abort("BSTS only supports a single trend argument")
  #   }
  #
  #   trend <- specials$trend[[1]]
  #
  #   if (trend$type == "local") {
  #     state <- AddLocalLinearTrend(
  #       state.specification = state,
  #       y = vec_data
  #     )
  #   } else if (trend$type == "semilocal") {
  #     state <- AddSemilocalLinearTrend(
  #       state.specification = state,
  #       y = vec_data
  #     )
  #   } else if (trend$type == "studentlocal") {
  #     state <- AddStudentLocalLinearTrend(
  #       state.specification = state,
  #       y = vec_data,
  #       save.weights = FALSE
  #     )
  #   }
  # }

  #-------------------------------------------------------------------------------------------------
  # SEASONALITY
  #-------------------------------------------------------------------------------------------------

  # if ("season" %in% names(specials)) {
  #   # Compute number of seasons
  #   periods <- common_periods(self$data)
  #   nseasons <- get_frequencies(period, self$data, .auto = "smallest")
  #   if (nseasons %in% periods) {
  #     name <- names(periods)[which(periods == nseasons)]
  #   } else {
  #     name <- paste0("season_", nseasons)
  #   }
  #
  #   for (season in specials$season) {
  #     #---------------------------------------------------------------------------------------------
  #     # Regression Seasonality
  #
  #     if (season$type == "regression") {
  #       # check validity
  #       if (!"period" %in% names(season)) {
  #         abort("period must be defined for regression seasonality.")
  #       }
  #
  #       state <- AddSeasonal(
  #         state.specification = state,
  #         y = vec_data,
  #         nseasons = season$nseasons
  #       )
  #
  #     #---------------------------------------------------------------------------------------------
  #     # Trigonometric Seasonality
  #
  #     } else if (season$type == "trig") {
  #       # check validity
  #       if (!"period" %in% names(season) || !"frequencies" %in% names(season)) {
  #         abort("period and frequencies must be defined for trig seasonality.")
  #       }
  #       if (!season$period > 0 || any(!frequencies > 0)) {
  #         abort("period and frequencies must be positive for trig seasonality.")
  #       }
  #
  #       state <- AddTrig(
  #         state.specification = state,
  #         y = vec_data,
  #         period = season$period,
  #         frequencies = season$frequencies
  #       )
  #
  #     #---------------------------------------------------------------------------------------------
  #     # Monthly Annual Cyclicality
  #
  #     } else if (season$type == "monthlyannual") {
  #       state <- AddMonthlyAnnualCycle(state)
  #     }
  #   }
  # }


  #-------------------------------------------------------------------------------------------------
  # HOLIDAYS
  #-------------------------------------------------------------------------------------------------

  # if ("holiday" %in% names(specials)) {
  #   holiday <- specials$holiday[[1]]
  #   for (holiday in specials$holiday) {
  #     holiday_type <- trimws(tolower(holiday$type))
  #
  #     if (is_missing(holiday_type) || holiday_type %in% c("reg", "regression")) {
  #       state <- AddRegressionHoliday(
  #         state.specification = state,
  #         y = vec_data,
  #         holiday.list = holiday$holidays_list,
  #         time0 = holiday$first_observation,
  #         prior = holiday$prior)
  #     } else if (holiday_type %in% c("randomwalk", "rw")) {
  #       state <- AddRandomWalkHoliday(
  #         state.specification = state,
  #         y = vec_data,
  #         holiday = holiday$holidays_list,
  #         time0 = holiday$first_observation,
  #         sigma.prior = holiday$sigma_prior,
  #         initial.state.prior = holiday$initial_state_prior
  #       )
  #     } else if (holiday_type %in% c("hierarchical", "hierarchicalregression", "hr", "hreg")) {
  #       state <- AddHierarchicalRegressionHoliday(
  #         state.specification = state,
  #         y = vec_data,
  #         holiday.list = holiday$holidays_list,
  #         coefficient.mean.prior = holiday$coefficient_mean_prior,
  #         coefficient.variance.prior = holiday$coefficient_variance_prior,
  #         time0 = holiday$first_observation
  #       )
  #     }
  #   }
  # }


  #-------------------------------------------------------------------------------------------------
  # EXOGENOUS REGRESSORS
  #-------------------------------------------------------------------------------------------------

  # if ("xreg" %in% names(specials)) {
  #   xreg_data <-
  #     if (nrow(xreg_data) != length(vec_data)) {
  #       abort("The number of observations in ")
  #     }
  #
  #   for(regressor in specials$xreg){
  #     for(nm in colnames(regressor$xreg)){
  #       model_data[nm] <- regressor$xreg[,nm]
  #
  #       if (nrow(xreg_data) != length(vec_data)) {
  #         abort("The number of observations in ")
  #       }
  #
  #
  #       state <- bsts::AddDynamicRegression(
  #         state, name = nm, )
  #     }
  #   }
  # }





  # Train model
  mdl <- bsts::bsts(
    state.specification = state,
    family = family,
    data = xts_data,
    prior = prior,
    niter = iterations
  )
  fits <- predict(mdl, model_data)

  # Return model
  structure(
    list(
      model = mdl,
      est = list(.fitted = fits$yhat, .resid = vec_data - fits$yhat),
      components = .data %>% mutate(!!!(fits[c("trend", names(mdl$seasonalities))]))),
    class = "fbl_bsts")
}

#===================================================================================================
#
# SPECIALS
#
#===================================================================================================

specials_bsts <- new_specials(
  intercept = function() {
    as.list(environment())
  }
  ,ar = function(lags = NULL) {
    type <- match.arg(type)
    as.list(environment())
  }
  ,level = function(type = c("local", "shared")) {
    type <- match.arg(type)
    as.list(environment())
  }
  ,trend = function(type = c("local", "semilocal", "studentlocal")){
    type <- match.arg(type)
    as.list(environment())
  }
  # ,season = function(type = c("regression", "trig", "monthlyannual"), period = NULL){
  #   type <- match.arg(type)
  #   as.list(environment())
  # }
  # ,holiday = function(type = c("regression", "randomwalk", "hierarchical"),
  #                    holiday_tbl = NULL, holiday_names = NULL) {
  #   type <- match.arg(type)
  #   as.list(environment())
  # }
  # ,xreg = function(..., lags = 1, standardize = "auto", type = NULL){
  #   model_formula <- new_formula(
  #     lhs = NULL,
  #     rhs = reduce(c(0, enexprs(...)), function(.x, .y) call2("+", .x, .y))
  #   )
  #   list(
  #     xreg = model.matrix(model_formula, self$data),
  #     prior_scale = prior_scale,
  #     standardize = standardize,
  #     mode = type
  #   )
  # }
)

#' bsts procedure modelling
#'
#' Prepares a bsts model specification for use within the `fable` package.
#'
#' The bsts modelling interface uses a `formula` based model specification
#' (`y ~ x`), where the left of the formula specifies the response variable,
#' and the right specifies the model's predictive terms. Like any model in the
#' fable framework, it is possible to specify transformations on the response.
#'
#' A bsts model supports local and semi-local linear trends, local levels
#' with a random walk trend, additive seasonality and exogenous regressors.
#' These can be specified using the 'specials' functions detailed
#' below. The introduction vignette provides more details on how to model data
#' using this interface to prophet: `vignette("intro", package="fable.bsts")`.
#'
#' @param formula A symbolic description of the model to be fitted of class `formula`.
#' @inheritParams bsts::bsts
#'
#' @section Specials:
#'
#' \subsection{intercept}{
#' The `intercept` special is used to specify the intercept parameters.
#' \preformatted{
#' intercept()
#' }
#' }
#'
#' \subsection{ar}{
#' The `ar` special is used to specify the autoregressive parameters.
#' \preformatted{
#' ar(lag = NULL)
#' }
#'
#' \tabular{ll}{
#'   `lag`  \tab The number of lags ("p") in the AR(p) process.\cr
#' }
#' }
#'
#' \subsection{level}{
#' The `level` special is used to specify the level parameters.
#' \preformatted{
#' level(type = c("local", "shared"))
#' }
#'
#' \tabular{ll}{
#'   `type`  \tab The type of level (local or shared).\cr
#' }
#' }
#'
#' \subsection{trend}{
#' The `trend` special is used to specify the trend parameters.
#' \preformatted{
#' trend(type = c("locallinear", "semilocallinear", "studentlocallinear"))
#' }
#'
#' \tabular{ll}{
#'   `type`  \tab The type of trend (local linear, semi-local linear or student local linear).\cr
#' }
#' }
#'
#' \subsection{season}{
#' The `season` special is used to specify a seasonal component.
#' This special can be used multiple times for different seasonalities.
#'
#' **Warning: The inputs controlling the seasonal `period` is different than [`bsts::bsts()`]. Numeric inputs are treated as the number of observations in each seasonal period, not the number of days.**
#'
#' \preformatted{
#' season(type = c("regression", "trig", "monthlyannual"), period = NULL)
#' }
#'
#' \tabular{ll}{
#'   `type`  \tab The type of seasonality (seasonal regression, trigonometric, or monthly-annual cycle).\cr
#'   `period`   \tab The periodic nature of the seasonality. If a number is given, it will specify the number of observations in each seasonal period. If a character is given, it will be parsed using `lubridate::as.period`, allowing seasonal periods such as "2 years".\cr
#' }
#' }
#'
#' \subsection{holiday}{
#' The `holiday` special is used to specify a `tsibble` containing holidays for the model.
#' \preformatted{
#' holiday(holidays = NULL)
#' }
#'
#' \tabular{ll}{
#'   `holidays`   \tab A [`tsibble`](https://tsibble.tidyverts.org/) containing a set of holiday events. The event name is given in the 'holiday' column, and the event date is given via the index. Additionally, "lower_window" and "upper_window" columns can be used to include days before and after the holiday.\cr
#' }
#' }
#'
#' \subsection{xreg}{
#' The `xreg` special is used to include exogenous regressors in the model. This special can be used multiple times for different regressors with different arguments.
#' Exogenous regressors can also be used in the formula without explicitly using the `xreg()` special, which will then use the default arguments.
#' \preformatted{
#' xreg(..., prior_scale = NULL, standardize = "auto", type = NULL)
#' }
#'
#' \tabular{ll}{
#'   `...`         \tab A set of bare expressions that are evaluated as exogenous regressors\cr
#'   `standardize` \tab Should the regressor be standardised before fitting? If "auto", it will standardise if the regressor is not binary.\cr
#' }
#' }
#'
#' @seealso
#' - [`bsts::bsts()`]
#' - [bsts homepage](https://sites.google.com/view/stevethebayesian/software)
#' - [bsts R package](https://CRAN.R-project.org/package=bsts)
#'
#' @examples
#'
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_production %>%
#'   model(
#'     bsts = BSTS(Beer ~ season("year"))
#'   )
#' }
#'
#' @export
BSTS <- function(formula, ...){
  bsts_model <- new_model_class("bsts", train_bsts, specials_bsts)
  new_model_definition(bsts_model, !!enquo(formula), ...)
}

#' Produce forecasts from the bsts model
#'
#' If additional future information is required (such as exogenous variables)
#' by the model, then they should be included as variables of the `new_data` argument.
#'
#' @inheritParams fable::forecast.ARIMA
#' @param ... Additional arguments passed to [`bsts::predict.bsts()`].
#'
#' @seealso [`bsts::predict.bsts()`]
#'
#' @return A list of forecasts.
#'
#' @examples
#'
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_production %>%
#'   model(
#'     bsts = BSTS(Beer ~ season("year"))
#'   ) %>%
#'   forecast()
#' }
#'
#' @export
forecast.fbl_bsts <- function(object, new_data, specials = NULL, iterations = 1000, ...){
  mdl <- object$model

  # Prepare data
  new_data <- rename(as.data.frame(new_data), ds = !!index(new_data))

  ## trend
  trend <- specials$trend[[1]]

  # ## Exogenous Regressors
  # for(regressor in specials$xreg){
  #   for(nm in colnames(regressor$xreg)){
  #     new_data[nm] <- regressor$xreg[,nm]
  #   }
  # }

  # Compute predictions without intervals
  mdl$uncertainty.samples <- 0
  pred <- predict(mdl, new_data)

  # Simulate future paths
  mdl$niter <- iterations
  sim <- prophet::predictive_samples(mdl, new_data, ...)$yhat
  sim <- split(sim, row(sim))

  # Return forecasts
  construct_fc(pred$yhat, unname(map_dbl(sim, stats::sd)), dist_sim(sim))
}

#' Extract fitted values
#'
#' Extracts the fitted values from an estimated Prophet model.
#'
#' @inheritParams fable::fitted.ARIMA
#'
#' @return A vector of fitted values.
#'
#' @export
fitted.fbl_prophet <- function(object, ...){
  object$est[[".fitted"]]
}

#' Extract model residuals
#'
#' Extracts the residuals from an estimated bsts model.
#'
#' @inheritParams fable::residuals.ARIMA
#'
#' @return A vector of residuals.
#'
#' @export
residuals.fbl_bsts <- function(object, ...){
  object$est[[".resid"]]
}

#' Extract meaningful components
#'
#' A bsts model consists of terms which are additively included in the model.
#'
#' Extracting a bsts model's components using this function allows you to
#' visualise the components in a similar way to [`bsts::plotBstsComponents()`].
#'
#' @inheritParams fable::components.ETS
#'
#' @return A [`fabletools::dable()`] containing estimated states.
#'
#' @examples
#'
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' beer_components <- tsibbledata::aus_production %>%
#'   model(
#'     bsts = BSTS(Beer ~ season("year"))
#'   ) %>%
#'   components()
#'
#' beer_components
#'
#' \dontrun{
#' autoplot(beer_components)
#'
#' library(ggplot2)
#' library(lubridate)
#' beer_components %>%
#'   ggplot(aes(x = quarter(Quarter), y = year, group = year(Quarter))) +
#'   geom_line()
#' }
#' }
#'
#' @export
components.fbl_bsts <- function(object, ...){
  cmp <- object$components
  cmp$.resid <- object$est$.resid
  mv <- measured_vars(cmp)
  as_dable(cmp, resp = !!sym(mv[1]), method = "bsts",
           aliases = set_names(
             list(expr(!!sym("trend") * (1 + !!sym("multiplicative_terms")) + !!sym("additive_terms") + !!sym(".resid"))),
             mv[1]
           )
  )
}

#' Glance a prophet model
#'
#' A glance of a prophet provides the residual's standard deviation (sigma), and
#' a tibble containing the selected changepoints with their trend adjustments.
#'
#' @inheritParams fable::glance.ARIMA
#'
#' @return A one row tibble summarising the model's fit.
#'
#' @examples
#'
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' fit <- tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' glance(fit)
#' }
#'
#' @export
glance.fbl_prophet <- function(x, ...){
  changepoints <- tibble(
    changepoints = x$model$changepoints,
    adjustment = as.numeric(x$model$params$delta)
  )
  tibble(sigma = stats::sd(x$est$.resid, na.rm = TRUE), changepoints = list(changepoints))
}

#' Extract estimated coefficients from a prophet model
#'
#' @inheritParams fable::tidy.ARIMA
#'
#' @return A tibble containing the model's estimated parameters.
#'
#' @examples
#'
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' fit <- tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' tidy(fit) # coef(fit) or coefficients(fit) can also be used
#' }
#'
#' @export
tidy.fbl_prophet <- function(x, ...){
  growth_terms <- c("base_growth", "trend_offset")

  seas_terms <- map2(
    x$model$seasonalities, names(x$model$seasonalities),
    function(seas, nm){
      k <- seas[["fourier.order"]]
      paste0(nm, rep(c("_s", "_c"), k), rep(seq_len(k), each = 2))
    }
  )

  hol_terms <- if (is.null(x$model$holidays)) {
    NULL
    } else {
      map2(
        x$model$holidays$holiday,
        map2(x$model$holidays[["lower_window"]]%||%0, x$model$holidays[["upper_window"]]%||%0, seq),
        function(nm, window){
          window <- ifelse(sign(window) == 1, paste0("_+", window), ifelse(sign(window) == -1, paste0("_", window), ""))
          paste0(nm, window)
        }
      )
    }

  xreg_terms <- names(x$model$extra_regressors)

  tibble(
    term = invoke(c, c(growth_terms, seas_terms, hol_terms, xreg_terms)),
    estimate = c(x$model$params$k, x$model$params$m, x$model$params$beta)
  )
}

#' @export
model_sum.fbl_prophet <- function(x){
  "prophet"
}

#' @export
format.fbl_prophet <- function(x, ...){
  "Prophet Model"
}
