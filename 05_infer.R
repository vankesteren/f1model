library(tidyverse)
library(brms)
library(glue)
library(ggridges)
library(firatheme)
library(patchwork)

fit <- read_rds("dat/fit.rds")

# Inference about driver talent ----
drivers_2021 <- c("hamilton", "bottas", "ricciardo", "norris", "sainz", "leclerc", "russell", "latifi",
                  "max_verstappen", "perez", "mick_schumacher", "mazepin", "ocon", "alonso", "raikkonen",
                  "giovinazzi", "vettel", "stroll", "tsunoda", "gasly")

driver_pars <- posterior_samples(fit, pars = "r_driver")
colnames(driver_pars) <- str_extract(colnames(driver_pars), "(?<=\\[).+(?=\\,Intercept])")
driver_talent_samples <-
  as_tibble(driver_pars) %>%
  pivot_longer(everything(), names_to = "Driver", values_to = "Talent")

driver_talent_summary <-
  driver_talent_samples %>%
  group_by(Driver) %>%
  summarise(
    est = mean(Talent),
    lower = quantile(Talent, 0.055),
    upper = quantile(Talent, 0.945),
  )

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

plt_driver + plt_constructor

ggsave("img/driver_constructor.png", width = 15, height = 9)

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
