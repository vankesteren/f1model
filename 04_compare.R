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
# Basic             -2.3       6.1
# Circuit + Weather -2.7       2.2
# Weather           -4.7       6.3

# Circuit works best, but not that different from basic

