# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-11-20 by @vankesteren
# Contents: Creating and estimating models
library(tidyverse)
library(brms)

# read data
f1_dat_finished  <- read_rds("dat/f1_dat_finished.rds")

# basic model
fit_basic <- brm(
  formula = prop_trans ~ 0 + (1 + n_years | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_basic)
write_rds(fit_basic, "fit/fit_basic.rds")

# basic nonlinear model
fit_basic_s <- brm(
  formula = prop_trans ~ 0 + (1 + poly(n_years, 2) | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_basic_s)
write_rds(fit_basic_s, "fit/fit_basic_s.rds")

# weather model
fit_weather <- brm(
  formula = prop_trans ~ 0 + (1 + n_years + weather_type | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather)
write_rds(fit_weather, "fit/fit_weather.rds")

# weather model with spline
fit_weather_s <- brm(
  formula = prop_trans ~ 0 + (1 + poly(n_years, 2) + weather_type | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather_s)
write_rds(fit_weather_s, "fit/fit_weather_s.rds")

# circuit type model
fit_circuit <- brm(
  formula = prop_trans ~ 0 + (1 + n_years | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_circuit)
write_rds(fit_circuit, "fit/fit_circuit.rds")

# circuit type model with spline
fit_circuit_s <- brm(
  formula = prop_trans ~ 0 + (1 + poly(n_years, 2) | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_circuit_s)
write_rds(fit_circuit_s, "fit/fit_circuit_s.rds")

# weather + circuit type model
fit_weather_circuit <- brm(
  formula = prop_trans ~ 0 + (1 + n_years + weather_type | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather_circuit)
write_rds(fit_weather_circuit, "fit/fit_weather_circuit.rds")


# weather + circuit type model with spline
fit_weather_circuit_s <- brm(
  formula = prop_trans ~ 0 + (1 + poly(n_years, 2) + weather_type | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather_circuit_s)
write_rds(fit_weather_circuit_s, "fit/fit_weather_circuit_s.rds")
