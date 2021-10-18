# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(brms)

options(mc.cores = 12)

# which model is best? Compare using LOO
fit_basic               <- read_rds("dat/fit_basic.rds") %>% add_criterion("loo")
fit_circuit             <- read_rds("dat/fit_circuit.rds") %>% add_criterion("loo")
fit_weather             <- read_rds("dat/fit_weather.rds") %>% add_criterion("loo")
fit_weather_circuit     <- read_rds("dat/fit_weather_circuit.rds") %>% add_criterion("loo")
fit_driver_form         <- read_rds("dat/fit_driver_form.rds") %>% add_criterion("loo")
fit_driver_form_weather <- read_rds("dat/fit_driver_form_weather.rds") %>% add_criterion("loo")


loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_weather,
  fit_weather_circuit,
  fit_driver_form,
  fit_driver_form_weather,
  model_names = c("Basic", "Circuit", "Weather", "Weather + Circuit", "Driver form", "Driver form + Weather")
)

loo_results

write_rds(loo_results, "dat/loo_results.rds")

#                     elpd_diff  se_diff
# Driver form + Weather  0.0       0.0
# Weather               -1.1       2.5
# Driver form           -1.2       2.3
# Basic                 -1.4       3.4
# Weather + Circuit     -1.8       2.6
# Circuit               -3.4       3.4

# model including weather works best, but is similar to basic model
