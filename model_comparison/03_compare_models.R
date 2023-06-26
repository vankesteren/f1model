# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-14 by @vankesteren
# Contents: visually comparing models for formula one race results
library(tidyverse)
library(ggridges)
library(cmdstanr)
library(firatheme)
library(patchwork)
library(loo)

# Load data ----
f1_dat <- read_rds("dat/f1_dat.rds") |> filter(finished)

drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

teams_focus <- c("mercedes", "red_bull", "ferrari", "williams", "mclaren", "toro_rosso")

recode_constructors <- c(
  "Ferrari" = "ferrari",
  "McLaren" = "mclaren",
  "Mercedes" = "mercedes",
  "Red Bull" = "red_bull",
  "Toro Rosso" = "toro_rosso",
  "Williams" = "williams"
)


beta_fit  <- read_rds("model_comparison/fits/beta_fit.rds")
rank_fit  <- read_rds("model_comparison/fits/rank_fit.rds")
ar_fit    <- read_rds("model_comparison/fits/ar_fit.rds")
slope_fit <- read_rds("model_comparison/fits/slope_fit.rds")

# Comparing beta likelihood model to rank ordered logit likelihood ----
## Variance components ----
beta_sd_driver <- beta_fit$draws("tau_driver")
beta_sd_driver_season <- beta_fit$draws("tau_driver_season")
beta_sd_team <- beta_fit$draws("tau_team")
beta_sd_team_season <- beta_fit$draws("tau_team_season")
beta_var_driver <- beta_sd_driver^2 + beta_sd_driver_season^2
beta_var_team <- beta_sd_team^2 + beta_sd_team_season^2
beta_prop_var <- beta_var_team / (beta_var_team + beta_var_driver)

rank_sd_driver <- rank_fit$draws("tau_driver")
rank_sd_driver_season <- rank_fit$draws("tau_driver_season")
rank_sd_team <- rank_fit$draws("tau_team")
rank_sd_team_season <- rank_fit$draws("tau_team_season")
rank_var_driver <- rank_sd_driver^2 + rank_sd_driver_season^2
rank_var_team <- rank_sd_team^2 + rank_sd_team_season^2
rank_prop_var <- rank_var_team / (rank_var_team + rank_var_driver)

tibble(
  prop = c(beta_prop_var, rank_prop_var),
  model = as_factor(rep(c("Beta Model", "Rank-Ordered Logit Model"), each = 8000))
) |>
  ggplot(aes(x = prop, fill = model)) +
  geom_density(alpha = 0.6) +
  theme_fira() +
  labs(
    title = "Variance components",
    x = "Proportion of variance explained by constructor",
    fill = "", y = "Posterior density"
  ) +
  scale_fill_fira() +
  theme(legend.position = "top")

ggsave("model_comparison/img/variance.png", bg = "white", width = 9, height = 5)

# Driver plots ----
# beta model
beta_driver_skill <-
  beta_fit$draws("theta_driver", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = "driver_id",
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  )

beta_driver_form <-
  beta_fit$draws("theta_driver_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id])


beta_driver <-
  left_join(beta_driver_form, beta_driver_skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

beta_driver |>
  filter(driver_name %in% drivers_focus) |>
  ungroup() |>
  mutate(
    driver_name = fct_recode(driver_name, verstappen = "max_verstappen"),
    driver_name = fct_relabel(driver_name, str_to_title)
  ) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Beta model driver skills", colour = "Driver", fill = "Driver")

ggsave("model_comparison/img/beta_driver.png", bg = "white", width = 12, height = 8)



# rank model
rank_driver_skill <-
  rank_fit$draws("theta_driver", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = "driver_id",
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  )

rank_driver_form <-
  rank_fit$draws("theta_driver_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id])


rank_driver <-
  left_join(rank_driver_form, rank_driver_skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

rank_driver |>
  filter(driver_name %in% drivers_focus) |>
  ungroup() |>
  mutate(
    driver_name = fct_recode(driver_name, verstappen = "max_verstappen"),
    driver_name = fct_relabel(driver_name, str_to_title)
  ) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Rank model driver skills", colour = "Driver", fill = "Driver")


ggsave("model_comparison/img/rank_driver.png", bg = "white", width = 12, height = 8)



# direct comparison
bind_rows(
  rank_driver |> mutate(model = "rank"),
  beta_driver |> mutate(model = "beta")
) |>
  filter(driver_name %in% drivers_focus) |>
  ungroup() |>
  mutate(
    driver_name = fct_recode(driver_name, verstappen = "max_verstappen"),
    driver_name = fct_relabel(driver_name, str_to_title)
  ) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Driver skills comparison")


ggsave("model_comparison/img/driver_comparison.png", bg = "white", width = 12, height = 8)

# Team plots ----
# beta model
beta_team_skill <-
  beta_fit$draws("theta_team", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = "team_id",
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer
  )

beta_team_form <-
  beta_fit$draws("theta_team_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat |> pull(constructor))[team_id])

beta_team <-
  left_join(beta_team_form, beta_team_skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

beta_team |>
  filter(team_name %in% teams_focus) |>
  ungroup() |>
  mutate(team_name = fct_recode(team_name, !!!recode_constructors)) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Beta model team advantage", fill = "Constructor", colour = "Constructor")


ggsave("model_comparison/img/beta_team.png", bg = "white", width = 12, height = 8)

# rank model
rank_team_skill <-
  rank_fit$draws("theta_team", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = "team_id",
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer
  )

rank_team_form <-
  rank_fit$draws("theta_team_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat |> pull(constructor))[team_id])

rank_team <-
  left_join(rank_team_form, rank_team_skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

rank_team |>
  filter(team_name %in% teams_focus) |>
  ungroup() |>
  mutate(team_name = fct_recode(team_name, !!!recode_constructors)) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Rank model team advantage", fill = "Constructor", colour = "Constructor")


ggsave("model_comparison/img/rank_team.png", bg = "white", width = 12, height = 8)

# direct comparison
bind_rows(
  rank_team |> mutate(model = "rank"),
  beta_team |> mutate(model = "beta")
) |>
  filter(team_name %in% teams_focus) |>
  ungroup() |>
  mutate(team_name = fct_recode(team_name, !!!recode_constructors)) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Team advantage comparison")


ggsave("model_comparison/img/team_comparison.png", bg = "white", width = 12, height = 8)

# Compare AR and slope to rank model ----

# first, use LOO
loo_rank <- rank_fit$loo(cores = 10)
loo_ar <- ar_fit$loo(cores = 10)
loo_slope <- slope_fit$loo(cores = 10)

loo_compare(list(rank = loo_rank, ar = loo_ar, slope = loo_slope))

# the slope model is clearly worse, the AR model is similar to the rank model

# compare AR model & rank model for drivers
ar_driver <-
  ar_fit$draws("driver_skill", format = "draws_df") |>
  pivot_longer(
    starts_with("driver_skill"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "driver_skill\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id]) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

bind_rows(
  rank_driver |> mutate(model = "rank"),
  ar_driver |> mutate(model = "auto")
) |>
  filter(driver_name %in% drivers_focus) |>
  ungroup() |>
  mutate(
    driver_name = fct_recode(driver_name, verstappen = "max_verstappen"),
    driver_name = fct_relabel(driver_name, str_to_title)
  ) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Driver skills comparison")

ggsave("model_comparison/img/ar_rank_driver_comparison.png", bg = "white", width = 12, height = 8)

# compare AR model & rank model for teams
ar_team <-
  ar_fit$draws("team_contribution", format = "draws_df") |>
  pivot_longer(
    starts_with("team_contribution"),
    names_to = c("team_id", "season_num"),
    names_pattern = "team_contribution\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat |> pull(constructor))[team_id]) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

bind_rows(
  rank_team |> mutate(model = "rank"),
  ar_team |> mutate(model = "auto")
) |>
  filter(team_name %in% teams_focus) |>
  ungroup() |>
  mutate(team_name = fct_recode(team_name, !!!recode_constructors)) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Team advantage comparison")

ggsave("model_comparison/img/ar_rank_team_comparison.png", bg = "white", width = 12, height = 8)


# Bonus: show why the slope model is bad:
# Slope model
slope_driver_intercepts <-
  slope_fit$draws("theta_driver_intercept", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_drive"),
    names_to = "driver_id",
    names_pattern = "theta_driver_intercept\\[(\\d+)]",
    names_transform = as.integer
  )

slope_driver_slopes <-
  slope_fit$draws("theta_driver_slope", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_drive"),
    names_to = "driver_id",
    names_pattern = "theta_driver_slope\\[(\\d+)]",
    names_transform = as.integer
  )

slope_driver <-
  left_join(slope_driver_intercepts, slope_driver_slopes, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(season_num = list(1:8)) |>
  unnest_longer(season_num) |>
  mutate(value = value.x + (season_num - 5) * value.y) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id]) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

slope_driver |>
  filter(driver_name %in% drivers_focus) |>
  ungroup() |>
  mutate(
    driver_name = fct_recode(driver_name, verstappen = "max_verstappen"),
    driver_name = fct_relabel(driver_name, str_to_title)
  ) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Slope model driver skills", colour = "Driver", fill = "Driver")

ggsave("model_comparison/img/slope_driver.png", bg = "white", width = 12, height = 8)
# giovinazzi is suddenly the best?


