# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-14 by @vankesteren
# Contents: Counterfactual predictions
library(tidyverse)
library(cmdstanr)
library(posterior)
library(firatheme)

fit <- read_rds("fit/basic.rds")
f1_dat <- read_rds("dat/f1_dat.rds") |> filter(finished)

# in a race in 2020, how likely is it that raikkonen in a mercedes beats lewis in an alfa?
# raikkonen season number = 7, driver id = 7, team id = 1
rai_merc <- fit$draws(c(
  "theta_driver[7]",
  "theta_driver_season[7,7]",
  "theta_team[1]",
  "theta_team_season[1,7]"
), format = "df") |>
  select(starts_with("theta")) |>
  rowSums()

# hamilton-alfa season number = 7, driver id = 7, team id = 1
ham_alfa <- fit$draws(c(
  "theta_driver[19]",
  "theta_driver_season[19,7]",
  "theta_team[15]",
  "theta_team_season[15,7]"
), format = "df") |>
  select(starts_with("theta")) |>
  rowSums()

prob <- exp(ham_alfa) / (exp(ham_alfa) + exp(rai_merc))

ggplot(tibble(d = prob), aes(x = d)) +
  geom_density(fill = firaCols[4], alpha = 0.8) +
  geom_vline(xintercept = mean(prob)) +
  geom_rug(alpha = 0.05) +
  theme_fira() +
  labs(
    title = "Counterfactual prediction",
    subtitle = "Hamilton in Alfa Romeo versus Räikkönen in Mercedes",
    x = "Posterior Hamilton-Alfa win probability",
    y = "Density"
  )

ggsave("img/plt_counterfactual.png", width = 9, height = 4, bg = "white")


