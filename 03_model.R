# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Creating and estimating models
library(tidyverse)
library(brms)

# read data
f1_dat_finished  <- read_rds("dat/f1_dat_finished.rds")

# basic model
fit_basic <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_basic)
write_rds(fit_basic, "dat/fit_basic.rds")

# weather model
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

# circuit type model
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

# weather + circuit type model
fit_weather_circuit <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 + circuit_type | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_weather_circuit)
write_rds(fit_weather_circuit, "dat/fit_weather_circuit.rds")

# basic model, but with driver's nested in year (i.e driver form)
fit_driver_form <- brm(
  formula = prop_trans ~ 0 + (1 | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_driver_form)
write_rds(fit_driver_form, "dat/fit_driver_form.rds")

# weather model, but with driver's nested in year (i.e driver form)
fit_driver_form_weather <- brm(
  formula = prop_trans ~ 0 + (1 + weather_type | driver) + (1 | driver:year) + (1 | constructor) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  cores   = 4,
  threads = 3
)

summary(fit_driver_form_weather)
write_rds(fit_driver_form_weather, "dat/fit_driver_form_weather.rds")
