# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Inferences using posteriors of parameters
library(tidyverse)
library(brms)
library(firatheme)
library(patchwork)

fit <- read_rds("dat/fit_weather.rds")

# Inference about driver skill ----
drivers_2021 <- c("hamilton", "bottas", "ricciardo", "norris", "sainz", "leclerc", "russell", "latifi",
                  "max_verstappen", "perez", "mick_schumacher", "mazepin", "ocon", "alonso", "raikkonen",
                  "giovinazzi", "vettel", "stroll", "tsunoda", "gasly")

driver_pars <- posterior_samples(fit, pars = "r_driver\\[.+Intercept")
wet_pars    <- posterior_samples(fit, pars = "r_driver\\[.+weather_typewet")
colnames(driver_pars) <- colnames(wet_pars) <- str_extract(colnames(driver_pars), "(?<=\\[).+(?=\\,Intercept])")

driver_skill_samples <-
  as_tibble(driver_pars) %>%
  pivot_longer(everything(), names_to = "Driver", values_to = "Skill")

wet_skill_samples <-
  as_tibble(wet_pars + driver_pars) %>%
  pivot_longer(everything(), names_to = "Driver", values_to = "Skill")

driver_skill_summary <-
  driver_skill_samples %>%
  group_by(Driver) %>%
  summarise(
    est = mean(Skill),
    lower = quantile(Skill, 0.055),
    upper = quantile(Skill, 0.945),
  ) %>%
  arrange(-est)

wet_skill_summary <-
  wet_skill_samples %>%
  group_by(Driver) %>%
  summarise(
    est = mean(Skill),
    lower = quantile(Skill, 0.055),
    upper = quantile(Skill, 0.945),
  ) %>%
  arrange(-est)

plt_driver <-
  driver_skill_summary %>%
  filter(Driver %in% drivers_2021) %>%
  arrange(est) %>%
  mutate(Driver = as_factor(Driver)) %>%
  ggplot(aes(y = Driver)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), colour = firaCols[3]) +
  theme_fira() +
  labs(x = "Skill (log odds ratio)", title = "F1 driver skill",
       subtitle = "Average hybrid-era (2014-2020) driver skill,\naccounting for yearly constructor advantage.")

plt_driver_wet <-
  bind_rows(
    driver_skill_summary %>% filter(Driver %in% drivers_2021) %>% arrange(est) %>% mutate(weather_type = "Dry race"),
    wet_skill_summary %>% filter(Driver %in% drivers_2021) %>% mutate(weather_type = "Wet race")
  ) %>%
  mutate(Driver = as_factor(Driver)) %>%
  ggplot(aes(y = Driver, colour = weather_type)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), position = position_dodge(width = -.6)) +
  theme_fira() +
  scale_colour_manual(values = c(firaCols[3], firaCols[1])) +
  labs(x = "Skill (log odds ratio)", title = "F1 driver skill",
       subtitle = "Average hybrid-era (2014-2020) driver skill,\naccounting for yearly constructor advantage.",
       colour = "") +
  theme(legend.position = "top")

ggsave("img/driver_wet.png", width = 9, height = 9)


# Plot for constructor advantage ----
constructors_2021 <- c("alfa", "alphatauri", "renault", "ferrari", "haas", "mclaren", "mercedes", "racing_point",
                       "red_bull", "williams")

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
  filter(constructor %in% constructors_2021) %>%
  arrange(est) %>%
  mutate(constructor = as_factor(constructor)) %>%
  ggplot(aes(y = constructor)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), colour = firaCols[2]) +
  theme_fira() +
  labs(x = "Advantage (log odds ratio)", y = "Constructor", title = "F1 constructor advantage",
       subtitle = "Average hybrid-era (2014-2020) constructor advantage, \naccounting for driver skill & constructor form.")

plt_constructor

ggsave("img/constructor.png", width = 9, height = 7)

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
sd_samples <- posterior_samples(fit, "(sd|cor)_(driver|constructor)__(Intercept|weather_typewet)")
sd_samples <-
  sd_samples %>%
  mutate(
    sd_driver_dry  = sd_driver__Intercept,
    # use standard formula for variance of correlated random variables to get std.dev of drivers in wet
    sd_driver_wet  = sqrt(sd_driver_dry^2 + sd_driver__weather_typewet^2 +
                          2*cor_driver__Intercept__weather_typewet*sd_driver_dry*sd_driver__weather_typewet),
    sd_constructor = sd_constructor__Intercept
  ) %>%
  select(!contains("__")) %>%
  pivot_longer(everything(), names_to = "Component", values_to = "SD", names_prefix = "sd_")


ggplot(sd_samples, aes(x = SD, fill = Component)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0) +
  theme_fira() +
  scale_fill_fira() +
  xlim(0, 2.5) +
  labs(title = "Constructor contribution is larger than driver contributions",
       subtitle = "Based on constructor and driver random effect variance",
       y = "Posterior density",
       x = "Random effect standard deviation",
       fill = "RE component")


ggsave("img/variance.png", width = 9, height = 5)


# random effects standard deviation summary
sfit <- summary(fit, prob = 0.89, )
ranef_summary <- rbind(sfit$random$constructor, sfit$random$`constructor:year`, sfit$random$driver)[1:5, 1:4]
xtable::xtable(ranef_summary)
