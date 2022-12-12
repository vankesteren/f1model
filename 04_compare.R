# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-12-12 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(cmdstanr)
library(loo)
library(xtable)


# which model is best? Compare using LOO
fit_basic   <- read_rds("fit/basic.rds")
fit_weather <- read_rds("fit/weather.rds")
fit_circuit <- read_rds("fit/circuit.rds")
fit_all     <- read_rds("fit/weather_circuit.rds")

loo_basic   <- fit_basic$loo(cores = 11)
loo_weather <- fit_weather$loo(cores = 11)
loo_circuit <- fit_circuit$loo(cores = 11)
loo_all     <- fit_all$loo(cores = 11)


loo_results <- loo_compare(
  list(
    "Basic" = loo_basic,
    "Weather" = loo_weather,
    "Circuit" = loo_circuit,
    "Circuit + Weather" = loo_all
  )
)

loo_results

write_rds(loo_results, "fit/loo_results.rds")


xtable::xtable(loo_results)

#                   elpd_diff se_diff
# Circuit            0.0       0.0
# Circuit + Weather -0.6       1.8
# Basic             -2.5       6.2
# Weather           -5.4       6.2

# Circuit works best

