# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Creating and estimating models
library(tidyverse)
library(cmdstanr)

# read & prepare stan data
f1_dat <-
  read_rds("dat/f1_dat.rds") %>%
  filter(finished)

stan_data <- list(
  num_obs           = f1_dat %>% nrow(),
  num_drivers       = f1_dat %>% pull(driver) %>% nlevels(),
  num_teams         = f1_dat %>% pull(constructor) %>% nlevels(),
  num_races         = f1_dat %>% group_by(year, round) %>% n_groups(),
  num_seasons       = f1_dat %>% group_by(year) %>% n_groups(),
  ranked_driver_ids = f1_dat %>% arrange(year, round, position) %>% pull(driver) %>% as.integer(),
  ranked_team_ids   = f1_dat %>% arrange(year, round, position) %>% pull(constructor) %>% as.integer(),
  num_entrants      = f1_dat %>% group_by(year, round) %>% summarize(count = n()) %>% pull(count),
  season_id         = f1_dat %>% group_by(year, round) %>% summarize(y = factor(first(year))) %>% pull(y) %>% as.integer(),
  wet_weather       = f1_dat %>% group_by(year, round) %>% summarize(w = first(weather_type)) %>% pull(w) %>% as.integer() - 1L,
  prm_circuit       = f1_dat %>% group_by(year, round) %>% summarize(c = first(circuit_type)) %>% pull(c) %>% as.integer() - 1L
)

# basic model
mod_basic <- cmdstan_model("stan_models/rank_model.stan")
fit_basic <- mod_basic$sample(stan_data, chains = 8, parallel_chains = 8, iter_sampling = 1000, max_treedepth = 12)
fit_basic$save_object("fit/basic.rds")

# weather model
mod_weather <- cmdstan_model("stan_models/weather_model.stan")
fit_weather <- mod_weather$sample(stan_data, chains = 8, parallel_chains = 8, iter_sampling = 1000, max_treedepth = 12)
fit_weather$save_object("fit/weather.rds")

# circuit type model
mod_circuit <- cmdstan_model("stan_models/circuit_model.stan")
fit_circuit <- mod_circuit$sample(stan_data, chains = 8, parallel_chains = 8, iter_sampling = 1000, max_treedepth = 12)
fit_circuit$save_object("fit/circuit.rds")

# weather + circuit type model
# weather model
mod_all <- cmdstan_model("stan_models/weather_circuit_model.stan")
fit_all <- mod_all$sample(stan_data, chains = 8, parallel_chains = 8, iter_sampling = 1000, max_treedepth = 12)
fit_all$save_object("fit/weather_circuit.rds")
