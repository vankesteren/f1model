library(tidyverse)
library(cmdstanr)

# load data
f1_dat <-
  read_rds("dat/f1_dat.rds") |>
  mutate(
    status       = as_factor(status),
    constructor  = as_factor(constructor),
    driver       = as_factor(driver),
    weather_type = as_factor(weather_type),
    circuit_type = as_factor(circuit_type)
  ) |>
  filter() |>
  mutate(across(where(is.factor), fct_drop))

# Compute classified indicator
# exclude all collisions & non-finishes
compute_classified <- function(status) {
  out <- rep(FALSE, length(status))
  # anyone above the last person still running (finished or +n laps is classified)
  last_classified <- max(which(status == "Finished" | str_starts(status, "\\+")))
  out[1:last_classified] <- TRUE
  out
}

f1_dat <-
  f1_dat |>
  group_by(year, round) |>
  mutate(
    finished = compute_classified(status),
    position = ifelse(finished, position, NA),
    position_prop = ifelse(finished, (n() - position) / (n() - 1), NA),
    prop_trans = ifelse(finished, (position_prop * (n() - 1) + 0.5) / n(), NA)
  )


# Beta model ----
# translated from brms into stan
beta_mod <- cmdstan_model("stan_models/beta_model.stan")

stan_data_beta <- list(
  num_obs = f1_dat |> filter(finished) |> nrow(),
  num_drivers = f1_dat |> filter(finished) |> pull(driver) |> nlevels(),
  num_teams = f1_dat |> filter(finished) |> pull(constructor) |> nlevels(),
  num_seasons = f1_dat |> group_by(year) |> n_groups(),
  y = f1_dat |> filter(finished) |> pull(prop_trans),
  driver_id = f1_dat |> filter(finished) |> pull(driver) |> as.integer(),
  team_id = f1_dat |> filter(finished) |> pull(constructor) |> as.integer(),
  season_id = f1_dat |> filter(finished) |> pull(year) |> as.factor() |> as.integer()
)

beta_fit <- beta_mod$sample(stan_data_beta, parallel_chains = 4, max_treedepth = 14)

beta_fit$save_object("stan_models/fits/beta_fit.rds")

# ROL model ----
# rank-ordered logit implementation

rank_mod <- cmdstan_model("stan_models/rank_model.stan")

stan_data_rank <- list(
  num_obs = f1_dat |> filter(finished) |> nrow(),
  num_drivers = f1_dat |> filter(finished) |> pull(driver) |> nlevels(),
  num_teams = f1_dat |> filter(finished) |> pull(constructor) |> nlevels(),
  num_races = f1_dat |> filter(finished) |> group_by(year, round) |> n_groups(),
  num_seasons = f1_dat |> group_by(year) |> n_groups(),
  ranked_driver_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(driver) |> as.integer(),
  ranked_team_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(constructor) |> as.integer(),
  num_entrants = f1_dat |> filter(finished) |> group_by(year, round) |> summarize(count = n()) |> pull(count),
  season_id = f1_dat |> filter(finished) |> group_by(year, round) |> summarize(year = factor(first(year))) |> pull(year) |> as.integer()
)

rank_fit <- rank_mod$sample(stan_data_rank, parallel_chains = 4)

rank_fit$save_object("stan_models/fits/rank_fit.rds")





