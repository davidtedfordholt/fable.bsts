# library(bsts)
#
# state <- list()
#
# state <- bsts::AddLocalLevel(
#   state.specification = state,
#   y = rnorm(10, mean = 5, sd = 2.5),
#   sigma.prior = Boom::SdPrior(
#     sigma.guess = 3     # prior guess of value of standard deviation
#   ),
#   initial.state.prior = Boom::NormalPrior(
#     mu = 5,             # mean of the prior distribution
#     sigma = 3,          # standard deviation of prior dist
#     initial.value = 5   # initial value of parameter in MCMC
#   )
# )
#
