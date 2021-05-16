# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: data processing, status filtering, outcome computation, some EDA
library(tidyverse)
library(firatheme)
f1_dat <- read_rds("dat/f1_dat.rds")

# Data processing ----
# convert to factors
f1_dat <- f1_dat %>% mutate(
  status       = as_factor(status),
  constructor  = as_factor(constructor),
  driver       = as_factor(driver),
  weather_type = as_factor(weather_type),
  circuit_type = as_factor(circuit_type)
)

# exclude all collisions & non-finishes
compute_classified <- function(status) {
  out <- rep("not classified", length(status))
  # anyone above the last person still running (finished or +n laps is classified)
  last_classified <- max(which(status == "Finished" | str_starts(status, "\\+")))
  out[1:last_classified] <- "classified"
  out
}

# compute prop. of finished drivers beaten in each round
f1_dat_finished <-
  f1_dat %>%
  group_by(year, round) %>%
  mutate(classified = compute_classified(status)) %>%
  filter(classified == "classified") %>%
  mutate(
    position_prop = (n() - position) / (n() - 1),        # how many classified drivers did you beat?
    prop_trans = (position_prop * (n() - 1) + 0.5) / n() # https://stats.stackexchange.com/a/134297/116878
  ) %>%
  ungroup() %>%
  select(-classified)

write_rds(f1_dat_finished, "dat/f1_dat_finished.rds")

# Some EDA ----
# finish position
ggplot(f1_dat_finished, aes(x = factor(position))) +
  geom_bar(fill = firaCols[4]) +
  theme_fira() +
  labs(
    title = "Distribution of finish positions",
    subtitle = "F1 hybrid era (2014-2020)",
    x = "Finish position",
    y = "Count"
  )

ggsave("img/eda_finish_position.png", width = 9, height = 6)

# basic plot
f1_dat_finished %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi")) %>%
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

ggsave("img/eda_finish_drivers.png", width = 12, height = 9)

f1_dat_finished %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi"), year != 2014) %>%
  ggplot(aes(x = prop_trans, fill = driver)) +
  geom_density(alpha = 0.5, bw = 0.1) +
  theme_fira() +
  scale_fill_fira() +
  labs(
    x = "Smoothed proportion of drivers beaten",
    y = "Density",
    title = "Different drivers' results",
    subtitle = "Proportion of finished drivers beaten",
    fill = ""
  ) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 0.85)) +
  facet_wrap(~year)

ggsave("img/eda_finish_drivers_prop.png", width = 9, height = 6)
