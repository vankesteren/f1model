# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-11-20 by @vankesteren
# Contents: Inferences using posteriors of parameters
library(tidyverse)
library(brms)
library(firatheme)
library(patchwork)
library(glue)

fit <- read_rds("fit/fit_weather.rds")

# Inference about driver skill ----
drivers_2021 <- c("hamilton", "bottas", "ricciardo", "norris", "sainz", "leclerc", "russell", "latifi",
                  "max_verstappen", "perez", "mick_schumacher", "mazepin", "ocon", "alonso", "raikkonen",
                  "giovinazzi", "vettel", "stroll", "tsunoda", "gasly")

# extract all driver parameters
driver_intercepts <- as_draws_df(fit, variable = "r_driver\\[.+Intercept", regex = TRUE)
driver_slopes     <- as_draws_df(fit, variable = "r_driver\\[.+n_years", regex = TRUE)
driver_wet        <- as_draws_df(fit, variable = "r_driver\\[.+weather_typewet", regex = TRUE)

# drop unnecessary columns
driver_intercepts <- driver_intercepts %>% as_tibble() %>% select(!starts_with("."))
driver_slopes     <- driver_slopes %>% as_tibble() %>% select(!starts_with("."))
driver_wet        <- driver_wet %>% as_tibble() %>% select(!starts_with("."))

# rename nicely
drivers_ordered <- str_extract(colnames(driver_intercepts), "(?<=\\[).+(?=\\,Intercept])")
colnames(driver_intercepts) <- drivers_ordered
colnames(driver_slopes)     <- drivers_ordered
colnames(driver_wet)        <- drivers_ordered

# select only 2021 drivers
driver_intercepts <- driver_intercepts %>% select(contains(drivers_2021))
driver_slopes     <- driver_slopes %>% select(contains(drivers_2021))
driver_wet        <- driver_wet %>% select(contains(drivers_2021))

# find n_years for those drivers
f1_dat_finished <- read_rds("dat/f1_dat_finished.rds")
driver_nyears <-
  f1_dat_finished %>%
  select(driver, year) %>%
  group_by(driver) %>%
  summarize(n_years = nlevels(as_factor(year))) %>%
  filter(driver %in% drivers_2021)

# Transform to long format
driver_intercepts_long <-
  driver_intercepts %>%
  pivot_longer(everything(), names_to = "driver", values_to = "skill")

driver_slopes_long <-
  driver_slopes %>%
  pivot_longer(everything(), names_to = "driver", values_to = "skill")

driver_wet_long <-
  driver_wet %>%
  pivot_longer(everything(), names_to = "driver", values_to = "skill")

# put everything in one table
driver_skills_2021 <-
  driver_intercepts_long %>%
  left_join(driver_nyears) %>%
  mutate(inc_skill = driver_slopes_long$skill,
         wet_skill = driver_wet_long$skill,
         exp_dry_skill = skill + n_years*inc_skill,
         exp_wet_skill = exp_dry_skill + wet_skill)


# summarize for plotting
dry_skill_summary <-
  driver_skills_2021 %>%
  group_by(driver) %>%
  summarise(
    est = mean(exp_dry_skill),
    lower = quantile(exp_dry_skill, 0.055),
    upper = quantile(exp_dry_skill, 0.945),
  ) %>%
  arrange(-est)

wet_skill_summary <-
  driver_skills_2021 %>%
  group_by(driver) %>%
  summarise(
    est = mean(exp_wet_skill),
    lower = quantile(exp_wet_skill, 0.055),
    upper = quantile(exp_wet_skill, 0.945),
  ) %>%
  arrange(-est)

plt_driver <-
  dry_skill_summary %>%
  arrange(est) %>%
  mutate(driver = as_factor(driver)) %>%
  ggplot(aes(y = driver)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), colour = firaCols[3]) +
  theme_fira() +
  labs(x = "Skill (log odds ratio)", title = "F1 driver skill in 2021")

plt_driver_wet <-
  bind_rows(
    dry_skill_summary %>% arrange(est) %>% mutate(weather_type = "Dry race"),
    wet_skill_summary %>% mutate(weather_type = "Wet race")
  ) %>%
  mutate(driver = as_factor(driver)) %>%
  ggplot(aes(y = driver, colour = weather_type)) +
  geom_pointrange(aes(x = est, xmin = lower, xmax = upper), position = position_dodge(width = -.6)) +
  theme_fira() +
  scale_colour_manual(values = c(firaCols[3], firaCols[1])) +
  labs(x = "Skill (log odds ratio)", title = "F1 driver skill in 2021",
       colour = "") +
  theme(legend.position = "top")

ggsave("img/driver_wet.png", width = 9, height = 9, bg = "white")


# Plot for constructor advantage ----
constructors_2021 <- c("alfa", "alphatauri", "renault", "ferrari", "haas", "mclaren", "mercedes", "racing_point",
                       "red_bull", "williams")


# extract all constructor mean parameters
constructor_means <- as_draws_df(fit, variable = "r_constructor\\[.+Intercept", regex = TRUE)
constructor_means <- constructor_means %>% as_tibble() %>% select(!starts_with("."))
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

ggsave("img/constructor.png", width = 9, height = 7, bg = "white")

# Plot for constructor form per year
constructoryear_pars <- as_draws_df(fit, variable = "r_constructor:year\\[.+Intercept", regex = TRUE)
constructoryear_pars <- constructoryear_pars %>% as_tibble() %>% select(!starts_with("."))
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

ggsave("img/constructor_form.png", width = 9, height = 6, bg = "white")


# Driver versus constructor contributions ----
sd_samples <- as_draws_df(fit, "(sd|cor)_(driver|constructor)__(Intercept|weather_typewet|n_years)", regex = TRUE)
sd_samples <-
  sd_samples %>%
  select(!starts_with(".")) %>%
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
sfit <- summary(fit, prob = 0.89)
ranef_summary <- rbind(sfit$random$constructor, sfit$random$`constructor:year`, sfit$random$driver)[,1:4]
xtable::xtable(ranef_summary)
