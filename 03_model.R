library(tidyverse)
library(brms)
f1_dat_processed <- read_rds("dat/f1_dat_processed.rds")
f1_dat_finished  <- read_rds("dat/f1_dat_finished.rds")


fit <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_processed,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit)
write_rds(fit, "dat/fit_basic.rds")

fit_finished <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_finished)
write_rds(fit_finished, "dat/fit_finished.rds")


fit_weather <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather)
write_rds(fit_weather, "dat/fit_weather.rds")

fit_circuit <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_circuit)
write_rds(fit_circuit, "dat/fit_circuit.rds")

fit_all <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_all)
write_rds(fit_all, "dat/fit_all.rds")
