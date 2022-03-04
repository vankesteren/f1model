# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(brms)
library(xtable)

options(mc.cores = 12)

# which model is best? Compare using LOO
fit_basic   <- read_rds("fit/fit_basic.rds") %>% add_criterion("loo")
fit_circuit <- read_rds("fit/fit_circuit.rds") %>% add_criterion("loo")
fit_weather <- read_rds("fit/fit_weather.rds") %>% add_criterion("loo")
fit_all     <- read_rds("fit/fit_weather_circuit.rds") %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_weather,
  fit_all,
  model_names = c("Basic", "Circuit", "Weather", "Circuit + Weather")
)

loo_results

write_rds(loo_results, "fit/loo_results.rds")


xtable::xtable(loo_results)

#                   elpd_diff se_diff
# Basic              0.0       0.0
# Circuit           -1.0       0.5
# Weather           -1.2       1.3
# Circuit + Weather -1.9       1.4

# basic works best

# comparing basic to next best model
bayes_factor(fit_basic, fit_circuit)
