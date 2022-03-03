# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Counterfactual predictions
library(tidyverse)
library(firatheme)
library(brms)

fit <- read_rds("fit/fit_basic.rds")

# in a wet race in 2020, how likely is it that raikkonen in a mercedes beats lewis in an alfa?
raikkonen_mercedes <- posterior_predict(fit, tibble(
  year = 2021,
  constructor = "mercedes",
  driver = "raikkonen"
))

hamilton_alfa <- posterior_predict(fit, tibble(
  year = 2021,
  constructor = "alfa",
  driver = "hamilton"
))

delta <- hamilton_alfa - raikkonen_mercedes

ggplot(tibble(d = delta), aes(x = d)) +
  geom_density(fill = firaCols[4], alpha = 0.8) +
  geom_vline(xintercept = mean(delta)) +
  theme_fira() +
  labs(
    title = "Counterfactual prediction",
    subtitle = "Hamilton in Alfa Romeo versus Räikkönen in Mercedes",
    x = "Hamilton-Alfa advantage",
    y = "Density"
  )

ggsave("img/plt_counterfactual.png", width = 9, height = 5, bg = "white")


