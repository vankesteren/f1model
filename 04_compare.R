# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(brms)

options(mc.cores = 10)

# which model is best? Compare using LOO
fit_basic   <- read_rds("dat/fit_basic.rds") %>% add_criterion("loo")
fit_circuit <- read_rds("dat/fit_circuit.rds")  %>% add_criterion("loo")
fit_weather <- read_rds("dat/fit_weather.rds")  %>% add_criterion("loo")
fit_all     <- read_rds("dat/fit_all.rds")      %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_finished,
  fit_circuit,
  fit_weather,
  fit_all,
  model_names = c("Basic", "Circuit", "Weather", "Circuit + Weather")
)

loo_results

write_rds(loo_results, "dat/loo_results.rds")

#                    elpd_diff se_diff
# Weather            0.0       0.0
# Basic             -0.8       2.4
# Circuit + Weather -1.4       0.6
# Circuit           -2.3       2.4

# model including weather works best, but is similar to fit_finished.
