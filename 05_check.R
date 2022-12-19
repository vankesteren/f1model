# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-14 by @vankesteren
# Contents: MCMC validation, posterior predictive checks
library(tidyverse)
library(bayesplot)
library(posterior)
library(firatheme)

f1_dat <- read_rds("dat/f1_dat.rds") %>% filter(finished)
fit <- read_rds("fit/basic.rds")
draws <- fit$draws()

# MCMC mixing ----
draws %>%
  mcmc_trace(regex_pars = "tau") +
  theme_fira() +
  scale_colour_fira(guide = "none") +
  facet_wrap(vars(parameter), scales = "free", ncol = 1)
ggsave("img/chains.png", width = 6, height = 8, bg = "white")

# Rhat ----
rhats <- apply(draws, 3, rhat)
all(rhats < 1.01, na.rm = TRUE)

# Posterior predictive check code ----
# we need to be able to sample from gumbel distribution, see
# http://www.glicko.net/research/multicompetitor.pdf, equation 2
rgumbel <- function(theta) {
  theta - log(-log(runif(length(theta))))
}

# create a big dataframe with all skill-related parameters for everyone
driver_skill <-
  subset_draws(draws, "theta_driver") %>%
  as_draws_df() %>%
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id"),
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer,
    values_to = "driver_skill"
  )

driver_form <-
  subset_draws(draws, "theta_driver_season") %>%
  as_draws_df() %>%
  pivot_longer(
    starts_with("theta_driver_season"),
    names_to = c("driver_id", "season_num"),
    names_pattern = "theta_driver_season\\[(\\d+)\\,(\\d+)\\]",
    names_transform = as.integer,
    values_to = "driver_form"
  )

team_advantage <-
  subset_draws(draws, "theta_team") %>%
  as_draws_df() %>%
  pivot_longer(
    starts_with("theta_team"),
    names_to = c("team_id"),
    names_pattern = "theta_team\\[(\\d+)]",
    names_transform = as.integer,
    values_to = "team_advantage"
  )

team_form <-
  subset_draws(draws, "theta_team_season") %>%
  as_draws_df() %>%
  pivot_longer(
    starts_with("theta_team_season"),
    names_to = c("team_id", "season_num"),
    names_pattern = "theta_team_season\\[(\\d+)\\,(\\d+)\\]",
    names_transform = as.integer,
    values_to = "team_form"
  )

# join everything
driver_performance <- left_join(driver_form, driver_skill)
team_performance <- left_join(team_form, team_advantage)

simulate_race <- function(race_year, race_round, nsim = 200) {
  cat("Simulating season", race_year, "round", race_round, "\n")
  pred_tab <-
    f1_dat %>%
    filter(year == race_year, round == race_round) %>%
    select(year, round, driver, constructor) %>%
    mutate(driver_id = as.integer(driver), team_id = as.integer(constructor))

  pred_tab_long <- left_join(
    x = pred_tab,
    y = driver_performance %>% filter(season_num == race_year - 2013, .draw <= nsim),
    by = "driver_id"
  ) %>% left_join(
    y = team_performance %>% filter(season_num == race_year - 2013, .draw <= nsim),
    by = c("team_id", ".chain", ".iteration", ".draw", "season_num")
  )

  pred_tab_long %>%
    mutate(skill = driver_skill + driver_form + team_advantage + team_form,
           latent_performance = rgumbel(skill)) %>%
    group_by(.draw) %>%
    mutate(position = rank(-latent_performance)) %>%
    ungroup() %>%
    select(year, round, driver, constructor, driver_id, team_id, .draw, position)
}

simulate_season <- function(race_year, nsim = 200) {
  n_rounds <- f1_dat %>% filter(year == race_year) %>% pull(round) %>% max()
  lapply(1:n_rounds, simulate_race, race_year = race_year, nsim = nsim) %>%
    bind_rows()
}

pp_check_plot <- function(race_year, nsim = 200) {
  ordered_levels <-
    f1_dat %>%
    filter(year == race_year) %>%
    group_by(driver) %>%
    summarise(pos = mean(position)) %>%
    arrange(pos) %>%
    pull(driver) %>%
    as.character()

  yrep <-
    simulate_season(race_year, 2000) %>%
    mutate(origin = "simulated")

  y <-
    f1_dat %>%
    filter(year == race_year) %>%
    select(driver, position) %>%
    mutate(origin = "observed")

  bind_rows(y, yrep) %>%
    ggplot(aes(x = factor(position), fill = origin, group = origin)) +
    geom_bar(aes(y = after_stat(prop)), position = position_dodge(preserve = "single")) +
    facet_wrap(~factor(driver, levels = ordered_levels)) +
    theme_linedraw() +
    theme(legend.position = "top", axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = paste(race_year, "season posterior predictive check"),
      x = "Position",
      y = "",
      fill = ""
    )
}


# Posterior predictive check 2015 ----
pp_check_plot(2015, nsim = 1500)
ggsave("img/pp_check_rank_2015.png", width = 15, height = 12, bg = "white")

# Posterior predictive check 2019 ----
pp_check_plot(2019, nsim = 1500)
ggsave("img/pp_check_rank_2019.png", width = 15, height = 12, bg = "white")

# Posterior predictive check 2021 ----
pp_check_plot(2021, nsim = 1500)
ggsave("img/pp_check_rank_2021.png", width = 15, height = 12, bg = "white")

# Expected points ----
# posterior prediction of expected average points per race in a season
yrep <- simulate_season(2021, 8000)

transform_points <- function(pos) {
  points_lut <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0)
  points_lut[pmin(pos, length(points_lut))]
}

expected_points <-
  yrep |>
  mutate(points = transform_points(position)) |>
  group_by(.draw, driver) |>
  summarise(constructor = first(constructor), total = sum(points)) |>
  group_by(driver) |>
  summarise(constructor = first(constructor), exp_points = mean(total) / 22, lower = quantile(total, 0.045)  / 22, upper = quantile(total, 0.955)  / 22) |>
  arrange(-exp_points)

observed_points <-
  f1_dat %>%
  filter(year == 2021) %>%
  mutate(points = transform_points(position)) |>
  group_by(driver) |>
  summarise(constructor = first(constructor), obs_points = sum(points) / 22)

left_join(expected_points, observed_points)

