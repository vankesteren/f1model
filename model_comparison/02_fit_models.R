# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-09 by @vankesteren
# Contents: fitting different stan models and saving samples to the fits/ folder.
library(tidyverse)
library(cmdstanr)


# Load data ----
f1_dat <- read_rds("dat/f1_dat.rds") |> filter(finished)

# Beta model ----
# translated from brms into stan
beta_mod <- cmdstan_model("model_comparison/stan_models/beta_model.stan")

stan_data_beta <- list(
  num_obs     = f1_dat |> nrow(),
  num_drivers = f1_dat |> pull(driver) |> nlevels(),
  num_teams   = f1_dat |> pull(constructor) |> nlevels(),
  num_seasons = f1_dat |> group_by(year) |> n_groups(),
  y           = f1_dat |> pull(prop_trans),
  driver_id   = f1_dat |> pull(driver) |> as.integer(),
  team_id     = f1_dat |> pull(constructor) |> as.integer(),
  season_id   = f1_dat |> pull(year) |> as.factor() |> as.integer()
)

beta_fit <- beta_mod$sample(stan_data_beta, parallel_chains = 4, max_treedepth = 14, iter_sampling = 2000)
beta_fit$save_object("model_comparison/fits/beta_fit.rds")

# ROL model ----
# rank-ordered logit implementation
rank_mod <- cmdstan_model("model_comparison/stan_models/rank_model.stan")

stan_data_rank <- list(
  num_obs           = f1_dat |> nrow(),
  num_drivers       = f1_dat |> pull(driver) |> nlevels(),
  num_teams         = f1_dat |> pull(constructor) |> nlevels(),
  num_races         = f1_dat |> group_by(year, round) |> n_groups(),
  num_seasons       = f1_dat |> group_by(year) |> n_groups(),
  ranked_driver_ids = f1_dat |> arrange(year, round, position) |> pull(driver) |> as.integer(),
  ranked_team_ids   = f1_dat |> arrange(year, round, position) |> pull(constructor) |> as.integer(),
  num_entrants      = f1_dat |> group_by(year, round) |> summarize(count = n()) |> pull(count),
  season_id         = f1_dat |> group_by(year, round) |> summarize(year = factor(first(year))) |> pull(year) |> as.integer()
)

rank_fit <- rank_mod$sample(stan_data_rank, parallel_chains = 4, iter_sampling = 2000)
rank_fit$save_object("model_comparison/fits/rank_fit.rds")

# ROL model with AR ----
ar_mod <- cmdstan_model("model_comparison/stan_models/ar_model.stan")
ar_fit <- ar_mod$sample(stan_data_rank, parallel_chains = 4, iter_sampling = 2000)
ar_fit$save_object("model_comparison/fits/ar_fit.rds")

# ROL model with simple slopes ----
slope_mod <- cmdstan_model("model_comparison/stan_models/slope_model.stan")
slope_fit <- slope_mod$sample(stan_data_rank, parallel_chains = 4, iter_sampling = 2000)
slope_fit$save_object("model_comparison/fits/slope_fit.rds")

