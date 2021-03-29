#predict
library(tidyverse)
library(brms)

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


# how often would raikkonen in a mercedes beat max in a mclaren?
kimi  <- posterior_predict(fit, tibble(year = 2020, constructor = "mercedes", driver = "raikkonen"))
lewis <- posterior_predict(fit, tibble(year = 2020, constructor = "mclaren", driver = "hamilton"))

mean(kimi>lewis)
