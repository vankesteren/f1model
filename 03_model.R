library(tidyverse)
library(brms)
f1_dat <- read_rds("dat/f1_dat_finished.rds")

fit <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit)
write_rds(fit, "dat/fit.rds")


fit_weather <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather)
write_rds(fit_weather, "dat/fit.rds")
