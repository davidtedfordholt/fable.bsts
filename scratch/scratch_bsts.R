library(dplyr)
library(lubridate)
library(tsibble)
library(tsibbledata)
library(bsts)
library(fable)

.data <- nyc_bikes %>%
  index_by(day = as.Date(start_time)) %>%
  # tsibble::index_by(month = tsibble::yearmonth(Time)) %>%
  group_by_key() %>%
  summarise(trips = n()) %>%
  fill_gaps(trips = 0)

iterations <- 100
horizon <- 100

.data <- .data %>% filter(bike_id == 26301)

.data$var_1 <- runif(n = nrow(.data))
.data$var_2 <- runif(n = nrow(.data))
model_formula <- rlang::new_formula(lhs = NULL, rhs = rlang::call2("+", quote(var_1), quote(var_2)))
regressor <- list()
regressor$xreg <- model.matrix(model_formula, .data)

zoo_data <- zoo::zoo(
  x = dplyr::pull(.data[tsibble::measured_vars(.data)], 1),
  order.by = dplyr::pull(.data[tsibble::index_var(.data)], 1)
)

model_data <- as_tibble(.data)[c(index_var(.data), measured_vars(.data))][, 1:2]
colnames(model_data) <- c("index", "y")


# Initialize state specification
state <- list()


# EXOGENOUS REGRESSORS ---------------------------------------------------------------------------


for (regressor in specials$xreg) {
  for (nm in colnames(regressor$xreg)) {
    model_data[nm] <- regressor$xreg[,nm]

    if (nrow(xreg_data) != length(vec_data)) {
      rlang::abort("The number of observations in ")
    }

    state <- bsts::AddDynamicRegression(
      state.specification = state,
      formula = model_formula,
      data = model_data)
  }
}




# TRAIN MODEL ------------------------------------------------------------------------------------

# bsts_quietly <- purrr::quietly(bsts::bsts)
#
# mdl <- bsts_quietly(
#   vec_data,
#   state.specification = state,
#   niter = iterations
# )

mdl <- bsts::bsts(
  zoo_data,
  state.specification = state,
  niter = iterations,
  ping = 0
)



plot(mdl, "components")

pred <- predict(mdl, horizon = 50)
sim <- split(pred$distribution, col(pred$distribution))

fabletools::construct_fc(
  point = pred$mean,
  sd = apply(pred$distribution, 2, stats::sd),
  dist = fabletools::dist_sim(sim)
)

cmp <- cbind.data.frame(
  .data, t(colMeans(mdl$state.contributions))) %>%
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

# plot(pred)
