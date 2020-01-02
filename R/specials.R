# TREND - trend()

# AddStaticIntercept - type = NULL, initial_state_prior
# AddAr - “AR”, lags, sigma_prior, inisial_state_prior
# AddAutoAr - “AR”, max_lags, prior
# AddLocalLevel - “Level”, sigma_prior, initial_state_prior
# AddSharedLocalLevel - “SharedLevel”, data, nfactors, coefficient_prior, initial_state_prior
# AddLocalLinearTrend - “Local”, level_sigma_prior, slope_sigma_prior, initial_level_prior, initial_slope_prior
# AddSemilocalLinearTrend - “Semilocal”, level_sigma_prior, slope_mean_prior, slope_AR1_prior, slope_sigma_prior, initial_level_prior, initial_slope_prior
# AddStudentLocalLinearTrend - “Student”, level_sigma_prior, level_nu_prior, slope_sigma_prior, slope_nu_prior, initial_level_prior, initial_slope_prior

type <- trimws(tolower(type))

if (is_missing(type) || type %in% c("static", "intercept", "staticintercept")) {
  state <- AddStaticIntercept(state)
} else if (type == "autoar" ||
           (type == "ar" && is_missing(lags))) {
  state <- AddAutoAr(state)
} else if (type == "ar") {
  state <- AddAr(state)
} else if (type %in% c("level", "locallevel")) {
  state <- AddLocalLevel(state)
} else if (type %in% c("shared", "sharedlevel")) {
  state <- AddSharedLocalLevel(state)
} else if (type %in% c("locallinear", "linear")) {
  state <- AddLocalLinearTrend(state)
} else if (type %in% c("semi", "semilocal", "semi-local", "semilocallinear")) {
  state <- AddSemilocalLinearTrend(state)
} else if (type %in% c("student", "studentlocal", "studentlinear", "studentlocallinear")) {
  state <- AddStudentLocalLinearTrend(state)
}


# SEASONALITY - season()

# AddSeasonal - type = NULL, obs_per_season, effect_duration, sigma_prior, initial_state_prior
# AddTrig - "Trig", priod, frequencies, sigma_prior, initial_state_prior, method
# AddMonthlyAnnualCycle - "MonthlyAnnual", first_observation, sigma_prior, initial_state_prior

type <- trimws(tolower(type))

if (is_missing(type) || type %in% c("season", "seasonal")) {
  state <- AddSeasonal(state)
} else if (type %in% c("trig", "trigonometric", "harmonic")) {
  state <- AddTrig(state)
} else if (type %in% c("cycle", "monthlyannual", "monthlyannualcycle")) {
  state <- AddMonthlyAnnualCycle(state)
}


# EXOGENOUS - xreg()

# AddDynamicRegression - data, model_options, sigma_mean_prior, contrasts, na.action

state <- AddDynamicRegression(state, )


# HOLIDAY - holiday()

# AddRandomWalkHoliday - type = "RW", holidays, first_observation, sigma_prior, initial_state_prior
# AddRegressionHoliday - type = NULL, holidays, first_observation, prior
# AddHierarchicalRegressionHoliday - type = "Hierarchical", holidays, coefficient_mean_prior, coefficient_variance_prior, first_observation

type <- trimws(tolower(type))

if (is_missing(type) || type %in% c("reg", "regression")) {
  state <- AddRegressionHoliday(state)
} else if (type %in% c("randomwalk", "rw")) {
  state <- AddRandomWalkHoliday(state)
} else if (type %in% c("hierarchical", "hierarchicalregression", "hr", "hreg")) {
  state <- AddHierarchicalRegressionHoliday(state)
}
