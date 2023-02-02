# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Create different models for sensitivity analysis

# load packages
library(tidyverse)
library(cmdstanr)

# read & prepare stan data
files <- list.files("sensitivity_analysis/data/", full.names = TRUE)

for (filename in files) {
  # get name
  name <- str_extract(filename, "(?<=dat\\_)\\w+(?=\\.rds)")

  # load file
  f1_dat <- read_rds(filename)

  # transform to stan data
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

  # create model
  mod <- cmdstan_model("stan_models/basic_model.stan")
  fit <- mod$sample(stan_data, chains = 8, parallel_chains = 8, iter_sampling = 1250)
  fit$save_object(paste0("sensitivity_analysis/fit/fit_", name, ".rds"))

}

