# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-11-20 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(brms)

options(mc.cores = 12)

# which model is best? Compare using LOO
fit_basic     <- read_rds("fit/fit_basic.rds")             %>% add_criterion("loo")
fit_basic_s   <- read_rds("fit/fit_basic_s.rds")           %>% add_criterion("loo")
fit_weather   <- read_rds("fit/fit_weather.rds")           %>% add_criterion("loo")
fit_weather_s <- read_rds("fit/fit_weather_s.rds")         %>% add_criterion("loo")
fit_circuit   <- read_rds("fit/fit_circuit.rds")           %>% add_criterion("loo")
fit_circuit_s <- read_rds("fit/fit_circuit_s.rds")         %>% add_criterion("loo")
fit_wtr_cir   <- read_rds("fit/fit_weather_circuit.rds")   %>% add_criterion("loo")
fit_wtr_cir_s <- read_rds("fit/fit_weather_circuit_s.rds") %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic, fit_basic_s, fit_weather, fit_weather_s, fit_circuit, fit_circuit_s, fit_wtr_cir, fit_wtr_cir_s,
  model_names = c("Basic", "Quadratic", "Weather", "Weather + Quadratic", "Circuit", "Circuit + Quadratic",
                  "Weather + Circuit", "Weather + Circuit + Quadratic")
)

loo_results

write_rds(loo_results, "dat/loo_results.rds")

#                               elpd_diff se_diff
# Weather                        0.0       0.0
# Weather + Quadratic           -0.4       1.0
# Basic                         -1.4       2.2
# Quadratic                     -2.0       2.4
# Weather + Circuit             -2.3       0.7
# Circuit                       -2.3       2.2
# Weather + Circuit + Quadratic -2.7       1.1
# Circuit + Quadratic           -3.3       2.5

# model including weather works best! slightly better than quadratic model
