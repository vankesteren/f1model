library(tidyverse)
library(ggridges)
library(cmdstanr)
library(firatheme)
library(patchwork)

# load datasets
f1_dat_all         <- read_rds("sensitivity_analysis/data/f1_dat_all.rds")
f1_dat_excl_car    <- read_rds("sensitivity_analysis/data/f1_dat_excl_car.rds")
f1_dat_excl_driver <- read_rds("sensitivity_analysis/data/f1_dat_excl_driver.rds")
f1_dat_finished    <- read_rds("sensitivity_analysis/data/f1_dat_finished.rds")

# load fits
fit_all         <- read_rds("sensitivity_analysis/fit/fit_all.rds")
fit_excl_car    <- read_rds("sensitivity_analysis/fit/fit_excl_car.rds")
fit_excl_driver <- read_rds("sensitivity_analysis/fit/fit_excl_driver.rds")
fit_finished    <- read_rds("sensitivity_analysis/fit/fit_finished.rds")

# focus on subset of drivers / teams
drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

teams_focus <- c("mercedes", "red_bull", "ferrari", "williams", "mclaren", "toro_rosso")


# Driver plot ----
# Basic_1 model (i.e., No drivers removed)
get_driver_draws <- function(fit, dat) {
  skill <-
    fit$draws("theta_driver", format = "draws_df") |>
    pivot_longer(
      starts_with("theta_driver"),
      names_to = "driver_id",
      names_pattern = "theta_driver\\[(\\d+)]",
      names_transform = as.integer
    )
  form <-
    fit$draws("theta_driver_season", format = "draws_df") |>
    pivot_longer(
      starts_with("theta_driver"),
      names_to = c("driver_id", "season_num"),
      names_pattern = "theta_driver_season\\[(\\d+),(\\d+)]",
      names_transform = as.integer
    ) |>
    mutate(driver_name = levels(dat |> pull(driver))[driver_id])

  left_join(form, skill, by = c(".chain", ".iteration", ".draw", "driver_id")) |>
    mutate(value = value.x + value.y) |>
    select(-value.x, -value.y)
}

# huge comparison dataset
driver_data <-
  bind_rows(
    get_driver_draws(fit_all, f1_dat_all) |> mutate(model = "Complete data"),
    get_driver_draws(fit_excl_car, f1_dat_excl_car) |> mutate(model = "Car-related non-finishers removed"),
    get_driver_draws(fit_excl_driver, f1_dat_excl_driver) |> mutate(model = "Driver-related non-finishers removed"),
    get_driver_draws(fit_finished, f1_dat_finished) |> mutate(model = "Only finishers retained")
  ) |>
  mutate(model = as_factor(model))

first_year_per_driver <-
  f1_dat_all %>%
  group_by(driver) %>%
  summarize(first_year = first(year))

driver_data_sum <-
  driver_data |>
  group_by(model, driver_name, season_num) |>
  #filter(driver_name %in% drivers_focus) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945)) |>
  left_join(first_year_per_driver, by = c("driver_name" = "driver")) %>%
  filter(season_num + 2013 >= first_year)


driver_data_sum |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = model, fill = model)) +
  geom_ribbon(alpha = .2, colour = NA) +
  geom_line() +
  geom_point() +
  theme_fira() +
  scale_fill_fira() +
  scale_colour_fira() +
  facet_wrap(vars(driver_name)) +
  labs(
    title = "Sensitivity analysis for driver skills",
    colour = "Model", fill = "Model",
    x = "Season",
    y = "Driver skill"
  )

ggsave("sensitivity_analysis/img/driver_skills_comparison.png", bg = "white", width = 12, height = 8)


drivers_2021 <-
  f1_dat_finished %>%
  filter(year == 2021) %>%
  pull(driver) %>%
  unique() %>%
  as.character()

fct_order <-
  driver_data_sum |>
  filter(model == "Only finishers retained", season_num + 2013 == 2021) |>
  arrange(y) |>
  pull(driver_name)


driver_data_sum %>%
  ungroup() %>%
  filter(
    season_num + 2013 == 2021,
    driver_name %in% drivers_2021
  ) %>%
  mutate(driver_name = factor(driver_name, levels = fct_order)) %>%
  ggplot(aes(y = driver_name, x = y, xmin = ymin, xmax = ymax, colour = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  theme_fira() +
  scale_colour_fira() +
  labs(title = "2021 F1 driver skill sensitivity analysis",
       x = "Skill (log odds ratio)",
       y = "Driver",
       colour = "Model")

ggsave("sensitivity_analysis/img/driver_2021_comparison.png", bg = "white", width = 12, height = 8)

# Team plots ----

get_team_draws <- function(fit, dat) {
  skill <- fit$draws("theta_team", format = "draws_df") |>
    pivot_longer(
      starts_with("theta_team"),
      names_to = "team_id",
      names_pattern = "theta_team\\[(\\d+)]",
      names_transform = as.integer
    )
  form <- fit$draws("theta_team_season", format = "draws_df") |>
    pivot_longer(
      starts_with("theta_team"),
      names_to = c("team_id", "season_num"),
      names_pattern = "theta_team_season\\[(\\d+),(\\d+)]",
      names_transform = as.integer
    ) |>
    mutate(team_name = levels(dat |> pull(constructor))[team_id])

  left_join(form, skill, by = c(".chain", ".iteration", ".draw", "team_id")) |>
    mutate(value = value.x + value.y) |>
    select(-value.x, -value.y)
}

# huge comparison dataset
team_data <-
  bind_rows(
    get_team_draws(fit_all, f1_dat_all) |> mutate(model = "Complete data"),
    get_team_draws(fit_excl_car, f1_dat_excl_car) |> mutate(model = "Car-related non-finishers removed"),
    get_team_draws(fit_excl_driver, f1_dat_excl_driver) |> mutate(model = "Driver-related non-finishers removed"),
    get_team_draws(fit_finished, f1_dat_finished) |> mutate(model = "Only finishers retained")
  ) |>
  mutate(model = as_factor(model))

team_data_sum <-
  team_data |>
  group_by(model, team_name, season_num) |>
  filter(team_name %in% teams_focus) |>
  summarize(y = mean(value), ymin = quantile(value, 0.055), ymax = quantile(value, 0.945))

# direct comparison
team_data_sum |>
  ggplot(aes(x = season_num, y = y, ymin = ymin, ymax = ymax, colour = team_name, fill = team_name)) +
  geom_ribbon(alpha = .2, colour = NA) +
  geom_line() +
  geom_point() +
  theme_fira() +
  scale_fill_brewer(type = "qual", palette = 3) +
  scale_colour_brewer(type = "qual", palette = 3) +
  facet_wrap(vars(model)) +
  labs(
    title = "Sensitivity analysis for constructor advantage",
    colour = "Model", fill = "Model",
    x = "Season",
    y = "Constructor advantage"
  )

ggsave("sensitivity_analysis/img/constructor_advantage_comparison.png", bg = "white", width = 12, height = 8)

