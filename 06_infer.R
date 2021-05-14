library(tidyverse)
library(brms)
library(glue)
library(ggridges)
library(firatheme)
library(patchwork)

fit <- read_rds("dat/fit_weather.rds")

# Inference about driver talent ----
drivers_2021 <- c("hamilton", "bottas", "ricciardo", "norris", "sainz", "leclerc", "russell", "latifi",
                  "max_verstappen", "perez", "mick_schumacher", "mazepin", "ocon", "alonso", "raikkonen",
                  "giovinazzi", "vettel", "stroll", "tsunoda", "gasly")

driver_pars <- posterior_samples(fit, pars = "r_driver\\[.+Intercept")
wet_pars    <- posterior_samples(fit, pars = "r_driver\\[.+weather_typewet")
colnames(driver_pars) <- colnames(wet_pars) <- str_extract(colnames(driver_pars), "(?<=\\[).+(?=\\,Intercept])")

driver_talent_samples <-
  as_tibble(driver_pars) %>%
  pivot_longer(everything(), names_to = "Driver", values_to = "Talent")

wet_talent_samples <-
  as_tibble(wet_pars + driver_pars) %>%
  pivot_longer(everything(), names_to = "Driver", values_to = "Talent")

driver_talent_summary <-
  driver_talent_samples %>%
  group_by(Driver) %>%
  summarise(
    est = mean(Talent),
    lower = quantile(Talent, 0.055),
    upper = quantile(Talent, 0.945),
  ) %>%
  arrange(-est)

wet_talent_summary <-
  wet_talent_samples %>%
  group_by(Driver) %>%
  summarise(
    est = mean(Talent),
    lower = quantile(Talent, 0.055),
    upper = quantile(Talent, 0.945),
  ) %>%
  arrange(-est)

plt_driver <-
  driver_talent_summary %>%
  filter(Driver %in% drivers_2021) %>%
  arrange(est) %>%
  mutate(Driver = as_factor(Driver)) %>%
  ggplot(aes(y = Driver)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), colour = firaCols[3]) +
  theme_fira() +
  labs(x = "Talent (log odds ratio)", title = "F1 driver talent",
       subtitle = "Average hybrid-era (2014-2020) driver talent,\naccounting for yearly constructor advantage.")

plt_driver_wet <-
  bind_rows(
    driver_talent_summary %>% filter(Driver %in% drivers_2021) %>% arrange(est) %>% mutate(weather_type = "Dry race"),
    wet_talent_summary %>% filter(Driver %in% drivers_2021) %>% mutate(weather_type = "Wet race")
  ) %>%
  mutate(Driver = as_factor(Driver)) %>%
  ggplot(aes(y = Driver, colour = weather_type)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), position = position_dodge(width = -.6)) +
  theme_fira() +
  scale_colour_manual(values = c(firaCols[3], firaCols[1])) +
  labs(x = "Talent (log odds ratio)", title = "F1 driver talent",
       subtitle = "Average hybrid-era (2014-2020) driver talent,\naccounting for yearly constructor advantage.",
       colour = "") +
  theme(legend.position = "top")

ggsave("img/driver_wet.png", width = 9, height = 9)


# Plot for constructor advantage ----
constructor_means <- posterior_samples(fit, "r_constructor\\[.+\\,Intercept\\]")
colnames(constructor_means) <- str_extract(colnames(constructor_means), "(?<=\\[).+(?=\\,Intercept])")
constructor_means_samples <-
  constructor_means %>%
  pivot_longer(
    cols = everything(),
    names_to = "constructor",
    values_to = "Advantage"
  )
constructor_means_summary <-
  constructor_means_samples %>%
  group_by(constructor) %>%
  summarise(
    est   = mean(Advantage),
    lower = quantile(Advantage, 0.055),
    upper = quantile(Advantage, 0.945),
  ) %>%
  ungroup()

plt_constructor <-
  constructor_means_summary %>%
  arrange(est) %>%
  mutate(constructor = as_factor(constructor)) %>%
  ggplot(aes(y = constructor)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), colour = firaCols[2]) +
  theme_fira() +
  labs(x = "Advantage (log odds ratio)", y = "Constructor", title = "F1 constructor advantage",
       subtitle = "Average hybrid-era (2014-2020) constructor advantage, \naccounting for driver talent & constructor form.")

plt_constructor

ggsave("img/constructor.png", width = 9, height = 9)

# Plot for constructor form per year
constructoryear_pars <- posterior_samples(fit, "r_constructor:year")
colnames(constructoryear_pars) <- str_extract(colnames(constructoryear_pars), "(?<=\\[).+(?=\\,Intercept])")
constructoryear_samples <-
  constructoryear_pars %>%
  pivot_longer(
    cols = everything(),
    names_to = c("constructor", "year"),
    names_pattern = "(.+)\\_([0-9]{4})",
    values_to = "Advantage"
  )

constructoryear_summary <-
  constructoryear_samples %>%
  group_by(constructor, year) %>%
  summarise(
    est   = mean(Advantage),
    lower = quantile(Advantage, 0.055),
    upper = quantile(Advantage, 0.945),
  ) %>%
  ungroup()

constructor_form <- function(yr) {
  constructoryear_summary %>%
    filter(year == as.character(yr)) %>%
    arrange(est) %>%
    mutate(constructor = as_factor(constructor)) %>%
    ggplot(aes(y = constructor)) +
    geom_pointrange(aes(x = est, xmin = lower, xmax = upper)) +
    theme_fira() +
    labs(y = "Constructor", x = "Advantage",
         title = glue("F1 season {yr} constructor form"))
}

constructor_progress <- function(const) {
  constructoryear_summary %>%
    filter(constructor %in% const) %>%
    ggplot(aes(x = as.numeric(year), colour = constructor)) +
    geom_pointrange(aes(y = est, ymin = lower, ymax = upper), position = position_dodge(width = 0.3)) +
    geom_line(aes(y = est), position = position_dodge(width = 0.3)) +
    scale_colour_fira() +
    theme_fira() +
    labs(y = "Form", x = "Year", title = glue("Seasonal constructor form"))
}

constructor_progress(c("ferrari", "mclaren", "mercedes", "red_bull")) +
  scale_colour_manual(values = c("red", "dark orange", "grey", "dark blue"))

ggsave("img/constructor_form.png", width = 9, height = 6)

# Driver versus constructor contributions ----
sd_samples <- posterior_samples(fit, "sd_(driver|constructor)__(Intercept|weather_typewet)")

ggplot(sd_samples, aes(x = sd_constructor__Intercept^2 - (sd_driver__Intercept^2 + sd_driver__weather_typewet^2))) +
  geom_density(fill = firaCols[4]) +
  geom_vline(xintercept = 0) +
  theme_fira() +
  xlim(-4, 4) +
  labs(title = "Constructor contribution is larger than driver contributions",
       subtitle = "Based on constructor and driver random effect variance",
       y = "Posterior density",
       x = "Constructor effect variance - driver effect variance")


ggsave("img/variance.png", width = 9, height = 5)
