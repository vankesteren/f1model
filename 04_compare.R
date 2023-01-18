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

print(loo_results, simplify = FALSE)

#                   elpd_diff se_diff elpd_loo se_elpd_loo p_loo   se_p_loo looic   se_looic
# Circuit               0.0       0.0 -3990.7     84.0       206.3    14.4   7981.3   167.9
# Basic                -2.3       6.1 -3993.0     83.7       187.1    12.6   7986.0   167.4
# Circuit + Weather    -2.7       2.2 -3993.3     84.4       217.1    15.3   7986.6   168.8
# Weather              -4.7       6.3 -3995.3     84.0       197.2    13.7   7990.7   168.1

write_rds(loo_results, "fit/loo_results.rds")


xtable::xtable(loo_results)

#                   elpd_diff se_diff
# Circuit            0.0       0.0
# Basic             -2.3       6.1
# Circuit + Weather -2.7       2.2
# Weather           -4.7       6.3

# Circuit works best, but not that different from basic

