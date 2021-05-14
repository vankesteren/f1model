#predict
library(tidyverse)
library(brms)

fit <- read_rds("dat/fit_circuit.rds")

# create table for round 1 2021
tab <- tibble(
  year = 2021,
  constructor = rep(c("mercedes", "mclaren", "ferrari", "williams", "red_bull", "haas",
                      "renault", "alfa", "racing_point", "alphatauri"), each = 2),
  driver = c("hamilton", "bottas", "ricciardo", "norris", "sainz", "leclerc", "russell", "latifi",
             "max_verstappen", "perez", "mick_schumacher", "mazepin", "ocon", "alonso", "raikkonen",
             "giovinazzi", "vettel", "stroll", "tsunoda", "gasly")
)


driver_talent_summary %>%
  arrange(est) %>%
  filter(Driver %in% tab$driver) %>%
  mutate(Driver = as_factor(Driver)) %>%
  ggplot(aes(y = Driver)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper)) +
  theme_fira() +
  labs(x = "Talent", title = "F1 driver talent",
       subtitle = "Hybrid-era (2014-2020) driver talent,\naccounting for constructor advantage.")


tabc <- tibble(
  year = 2020,
  constructor = c("mercedes", "mclaren", "ferrari", "williams", "red_bull", "haas",
                      "renault", "alfa", "racing_point", "alphatauri"),
  driver = "giovinazzi"
)


# in a dry race in 2015, how likely is it that vettel in a mercedes beats lewis in a red bull?
vettel_mercedes <- posterior_predict(fit, tibble(
  year = 2015,
  constructor = "mercedes",
  driver = "vettel",
  weather_type = "dry"
))

hamilton_red_bull <- posterior_predict(fit, tibble(
  year = 2015,
  constructor = "red_bull",
  driver = "hamilton",
  weather_type = "dry"
))

mean(vettel_mercedes > hamilton_red_bull)
