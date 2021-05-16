# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(brms)

options(mc.cores = 12)

# which model is best? Compare using LOO
fit_basic   <- read_rds("dat/fit_basic.rds") %>% add_criterion("loo")
fit_circuit <- read_rds("dat/fit_circuit.rds") %>% add_criterion("loo")
fit_weather <- read_rds("dat/fit_weather.rds") %>% add_criterion("loo")
fit_all     <- read_rds("dat/fit_weather_circuit.rds") %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_weather,
  fit_all,
  model_names = c("Basic", "Circuit", "Weather", "Circuit + Weather")
)

loo_results

write_rds(loo_results, "dat/loo_results.rds")

#                   elpd_diff se_diff
# Weather            0.0       0.0
# Basic             -0.3       2.2
# Circuit + Weather -0.7       0.6
# Circuit           -2.3       2.3

# model including weather works best, but is similar to basic model
