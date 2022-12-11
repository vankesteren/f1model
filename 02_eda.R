# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-11 by @vankesteren
# Contents: status filtering, some EDA
library(tidyverse)
library(firatheme)

# Data loading ----
f1_dat <- read_rds("dat/f1_dat.rds")
f1_dat_finished <- f1_dat %>% filter(finished)

# Some EDA ----
# finish position
f1_dat_finished %>%
  ggplot(aes(x = factor(position))) +
  geom_bar(fill = firaCols[4]) +
  theme_fira() +
  labs(
    title = "Distribution of finish positions",
    subtitle = "F1 hybrid era (2014-2021)",
    x = "Finish position",
    y = "Count"
  )

ggsave("img/eda_finish_position.png", width = 9, height = 6, bg = "white")

# basic plot
f1_dat_finished %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi"), year > 2015) %>%
  ggplot(aes(x = factor(position), fill = driver)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  theme_fira() +
  scale_fill_fira() +
  labs(
    x = "Finish position",
    y = "Count",
    title = "Different drivers' finish positions",
    subtitle = "Conditional on finishing the race",
    fill = ""
  ) +
  theme(legend.position = "top") +
  facet_wrap(~year)

ggsave("img/eda_finish_drivers.png", width = 9, height = 6, bg = "white")

# average finish positions for 2021 season
f1_dat_finished %>%
  filter(year == 2021) %>%
  group_by(driver) %>%
  summarize(mean_position = mean(position, na.rm = TRUE), sem = sd(position, na.rm = TRUE) / sqrt(n())) %>%
  mutate(driver = fct_reorder(driver, -mean_position)) %>%
  ggplot(aes(y = driver,
             x = mean_position,
             xmin = mean_position - 2*sem,
             xmax = mean_position + 2*sem)) +
  geom_pointrange(size = .4) +
  theme_fira() +
  labs(
    y = "",
    x = "Position (mean ± 2⋅se)",
    title = "2021 Season Finish Positions",
    subtitle = "Conditional on finishing the race"
  )

ggsave("img/eda_finish_2021.png", width = 9, height = 6, bg = "white")
