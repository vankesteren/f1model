library(tidyverse)
#library(ggridges)
library(cmdstanr)
#library(firatheme)
library(patchwork)
#library(loo)

f1_dat_finished_1 <- read_rds("sensitivity_analysis/data/f1_dat_finished_1.rds") |> filter(finished)
f1_dat_finished_2 <- read_rds("sensitivity_analysis/data/f1_dat_finished_2.rds") |> filter(finished)
f1_dat_finished_3 <- read_rds("sensitivity_analysis/data/f1_dat_finished_3.rds") |> filter(finished)
f1_dat_finished_4 <- read_rds("sensitivity_analysis/data/f1_dat_finished_4.rds") |> filter(finished)


drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

teams_focus <- c("mercedes", "red_bull", "ferrari", "williams", "mclaren", "toro_rosso")

basic_1  <- read_rds("sensitivity_analysis/fit/basic_1.rds")
basic_2  <- read_rds("sensitivity_analysis/fit/basic_2.rds")
basic_3  <- read_rds("sensitivity_analysis/fit/basic_3.rds")
basic_4  <- read_rds("sensitivity_analysis/fit/basic_4.rds")

# Driver plots ----
# Basic_1 model (i.e., No drivers removed)

basic_1_driver_skill <-
  basic_1$draws("theta_driver", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = "driver_id",
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  )

basic_1_driver_form <-
  basic_1$draws("theta_driver_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id])


basic_1_driver <-
  left_join(basic_1_driver_form, basic_1_driver_skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_1_driver |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Rank model driver skills",
       subtitle = "Data: No drivers removed")

ggsave("sensitivity_analysis/img/basic_1_driver.png", bg = "white", width = 12, height = 8)


# Basic_2 model (i.e., car-related non-finishers removed)
basic_2_driver_skill <-
  basic_2$draws("theta_driver", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = "driver_id",
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  )

basic_2_driver_form <-
  basic_2$draws("theta_driver_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id])


basic_2_driver <-
  left_join(basic_2_driver_form, basic_2_driver_skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_2_driver |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Rank model driver skills",
       subtitle = "Data: Car-related non-finishers removed")


ggsave("sensitivity_analysis/img/basic_2_driver.png", bg = "white", width = 12, height = 8)

# Basic_3 model (driver-related non-finishers removed)
basic_3_driver_skill <-
  basic_3$draws("theta_driver", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = "driver_id",
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  )

basic_3_driver_form <-
  basic_3$draws("theta_driver_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id])


basic_3_driver <-
  left_join(basic_3_driver_form, basic_3_driver_skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_3_driver |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Rank model driver skills",
       subtitle = "Data: Driver-related non-finishers removed")

ggsave("sensitivity_analysis/img/basic_3_driver.png", bg = "white", width = 12, height = 8)

# Basic_4 model (Only finishers retained, original dataset)
basic_4_driver_skill <-
  basic_4$draws("theta_driver", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = "driver_id",
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  )

basic_4_driver_form <-
  basic_4$draws("theta_driver_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id])


basic_4_driver <-
  left_join(basic_4_driver_form, basic_4_driver_skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(driver_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_4_driver |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = driver_name, fill = driver_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Rank model driver skills",
       subtitle = "Data: Only finishers retained")

ggsave("sensitivity_analysis/img/basic_4_driver.png", bg = "white", width = 12, height = 8)


# direct comparison
bind_rows(
  basic_1_driver |> mutate(model = "No drivers removed"),
  basic_2_driver |> mutate(model = "Car-related non-finishers removed"),
  basic_3_driver |> mutate(model = "Driver-related non-finishers removed"),
  basic_4_driver |> mutate(model = "Only finishers retained"),
) |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(driver_name)) +
  labs(title = "Driver skills comparison")

ggsave("sensitivity_analysis/img/driver_comparison.png", bg = "white", width = 12, height = 8)


# Team plots ----
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
  labs(title = "Rank model constructor advantage",
       subtitle = "Data: No drivers removed")

ggsave("sensitivity_analysis/img/basic_1_team.png", bg = "white", width = 12, height = 8)

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
  labs(title = "Rank model constructor advantage",
       subtitle = "Data: Car-related non-finishers removed")

ggsave("sensitivity_analysis/img/basic_2_team.png", bg = "white", width = 12, height = 8)


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
  labs(title = "Rank model constructor advantage",
       subtitle = "Data: Driver-related non-finishers removed")

ggsave("sensitivity_analysis/img/basic_3_team.png", bg = "white", width = 12, height = 8)


# basic 4 model
basic_4_team_skill <-
  basic_4$draws("theta_team", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = "team_id",
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer
  )

basic_4_team_form <-
  basic_4$draws("theta_team_season", format = "draws_df") |>
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(team_name = levels(f1_dat_finished_2 |> pull(constructor))[team_id])

basic_4_team <-
  left_join(basic_4_team_form, basic_4_team_skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
  mutate(value = value.x + value.y) |>
  select(-value.x, -value.y) |>
  group_by(team_name, season_num) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

basic_4_team |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .5, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Rank model constructor advantage",
       subtitle = "Data: Finishers retained")

ggsave("sensitivity_analysis/img/basic_4_team.png", bg = "white", width = 12, height = 8)


# direct comparison
bind_rows(
  basic_1_team |> mutate(model = "No drivers removed"),
  basic_2_team  |> mutate(model = "Car-related non-finishers removed"),
  basic_3_team  |> mutate(model = "Driver-related non-finish removed"),
  basic_4_team |> mutate(model = "Only finishers retained")
) |>
  filter(team_name %in% teams_focus) |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .35, colour = NA) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  facet_wrap(~as_factor(team_name)) +
  labs(title = "Constructor advantage comparison")

ggsave("sensitivity_analysis/img/constructor_comparison.png", bg = "white", width = 12, height = 8)

