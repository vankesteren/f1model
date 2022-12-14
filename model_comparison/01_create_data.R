# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-09 by @vankesteren
# Contents: preparing data from main analysis for use in model comparison
library(tidyverse)

# load data from main folder
f1_dat <-
  read_rds("dat/f1_dat.rds") |>
  mutate(
    status       = as_factor(status),
    constructor  = as_factor(constructor),
    driver       = as_factor(driver),
    weather_type = as_factor(weather_type),
    circuit_type = as_factor(circuit_type)
  ) |>
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
  ) |>
  filter(finished)

write_rds(f1_dat, "model_comparison/dat/f1_dat.rds")
