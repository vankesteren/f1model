library(tidyverse)
library(ggridges)
library(cmdstanr)
library(firatheme)
library(patchwork)
library(loo)

f1_dat_finished_1 <- read_rds("sensitivity_analysis/data/f1_dat_finished_1.rds") |> filter(finished)
f1_dat_finished_2 <- read_rds("sensitivity_analysis/data/f1_dat_finished_2.rds") |> filter(finished)
f1_dat_finished_3 <- read_rds("sensitivity_analysis/data/f1_dat_finished_3.rds") |> filter(finished)


drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

teams_focus <- c("mercedes", "red_bull", "ferrari", "williams", "mclaren", "toro_rosso")

basic_1  <- read_rds("sensitivity_analysis/fit/basic_1.rds")
basic_2  <- read_rds("sensitivity_analysis/fit/basic_2.rds")
basic_3  <- read_rds("sensitivity_analysis/fit/basic_3.rds")


# Team plots ----
# beta model
basic_1_team_skill <-
  basic_1$draws("theta_team", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = "team_id",
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer
  )

basic_1_team_form <-
  basic_1$draws("theta_team_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat_finished_1 |> pull(constructor))[team_id])

basic_1_team <-
  left_join(basic_1_team_form, basic_1_team_skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_1_team |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "basic_1 model team advantage")

ggsave("model_comparison/img/beta_team.png", bg = "white", width = 12, height = 8)

# basic 2 model
basic_2_team_skill <-
  basic_2$draws("theta_team", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = "team_id",
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer
  )

basic_2_team_form <-
  basic_2$draws("theta_team_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat_finished_2 |> pull(constructor))[team_id])

basic_2_team <-
  left_join(basic_2_team_form, basic_2_team_skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_2_team |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Basic 2 model team advantage")

ggsave("model_comparison/img/rank_team.png", bg = "white", width = 12, height = 8)


# basic 3 model
basic_3_team_skill <-
  basic_3$draws("theta_team", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = "team_id",
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer
  )

basic_3_team_form <-
  basic_3$draws("theta_team_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat_finished_2 |> pull(constructor))[team_id])

basic_3_team <-
  left_join(basic_3_team_form, basic_3_team_skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_3_team |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Basic 3 model team advantage")

# direct comparison
bind_rows(
  basic_1_team |> mutate(model = "true_finish"),
  basic_2_team  |> mutate(model = "finish_n_plus"),
  basic_3_team  |> mutate(model = "acc_coll")
) |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Team advantage comparison")
