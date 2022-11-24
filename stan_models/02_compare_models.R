library(tidyverse)
library(cmdstanr)
library(patchwork)

# Load data ----
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

drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

teams_focus <- c("mercedes", "red_bull", "ferrari", "williams", "mclaren", "toro_rosso")

beta_fit <- read_rds("stan_models/fits/beta_fit.rds")
rank_fit <- read_rds("stan_models/fits/rank_fit.rds")

# Variance components ----
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
  prop = c(rank_prop_var, beta_prop_var),
  model = rep(c("rank", "beta"), each = 8000)
) |>
  ggplot(aes(x = prop, fill = model)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Comparing variance components",
    x = "Proportion of variance explained by team"
  )

ggsave("stan_models/img/variance.png", bg = "white", width = 9, height = 5)

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
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Beta model driver skills")

ggsave("stan_models/img/beta_driver.png", bg = "white", width = 12, height = 8)



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
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Rank model driver skills")


ggsave("stan_models/img/rank_driver.png", bg = "white", width = 12, height = 8)

# direct comparison
bind_rows(
  rank_driver |> mutate(model = "rank"),
  beta_driver |> mutate(model = "beta")
) |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Driver skills comparison")


ggsave("stan_models/img/driver_comparison.png", bg = "white", width = 12, height = 8)

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
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Beta model team skills")


ggsave("stan_models/img/beta_team.png", bg = "white", width = 12, height = 8)

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
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Rank model team skills")


ggsave("stan_models/img/rank_team.png", bg = "white", width = 12, height = 8)


# direct comparison
bind_rows(
  rank_team |> mutate(model = "rank"),
  beta_team |> mutate(model = "beta")
) |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Team skills comparison")


ggsave("stan_models/img/team_comparison.png", bg = "white", width = 12, height = 8)
