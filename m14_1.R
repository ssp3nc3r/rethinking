library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce; rm(WaffleDivorce)

dat <- list(
  N = length(d$Divorce),
  div_obs = d$Divorce,
  div_sd = d$Divorce.SE,
  R = d$Marriage,
  A = d$MedianAgeMarriage
)

require(rstan)
fit <- stan(file = "div_err.stan", 
            data = dat, 
            chains = 4, 
            iter = 300,
            cores = 4,
            control = list(max_treedepth = 12, 
                           adapt_delta = .95)
)

fit
