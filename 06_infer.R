# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-12-14 by @vankesteren
# Contents: Inferences using posteriors of parameters
library(tidyverse)
library(cmdstanr)
library(firatheme)
library(patchwork)
library(glue)

fit <- read_rds("fit/basic.rds")
f1_dat <- read_rds("dat/f1_dat.rds") %>% filter(finished)

# Inference about driver skill ----
drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

driver_mean <- fit$draws("theta_driver", format = "df") %>% select(-.chain, -.iteration)
driver_form <- fit$draws("theta_driver_season", format = "df") %>% select(-.chain,-.iteration)

first_year_per_driver <-
  f1_dat %>%
  group_by(driver) %>%
  summarize(first_year = first(year))

driver_mean_long <-
  driver_mean  %>%
  pivot_longer(-.draw, names_to = "driver_id", values_to = "Skill",
               names_pattern = "\\[(\\d+)]", names_transform = as.integer) %>%
  mutate(Driver = as_factor(levels(f1_dat$driver)[driver_id]))

driver_form_long <-
  driver_form %>%
  pivot_longer(-.draw, names_to = c("driver_id", "season_num"), values_to = "Form",
               names_pattern = "\\[(\\d+),(\\d+)\\]", names_transform = as.integer) %>%
  mutate(Driver = as_factor(levels(f1_dat$driver)[driver_id]),
         Year = season_num + 2013)

driver_samples <-
  left_join(driver_form_long, driver_mean_long, by = c("Driver", ".draw")) %>%
  mutate(skill_yr = Form + Skill)

driver_skill_summary <-
  driver_samples %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr),
    lower = quantile(skill_yr, 0.055),
    upper = quantile(skill_yr, 0.945),
  ) %>%
  left_join(first_year_per_driver, by = c("Driver" = "driver")) %>%
  filter(Year >= first_year) %>%
  select(-first_year)

plt_skill_trajectory <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Driver %in% drivers_focus) %>%
  mutate(Driver = fct_reorder(Driver, -est),
         Driver = fct_recode(Driver, verstappen = "max_verstappen", schumacher = "mick_schumacher"),
         Driver = fct_relabel(Driver, str_to_title)) %>%
  ggplot(aes(x = Year, y = est, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(fill = Driver), alpha = .2) +
  geom_line(aes(colour = Driver)) +
  geom_point(aes(colour = Driver)) +
  scale_fill_fira(guide = "none") +
  scale_colour_fira(guide = "none") +
  theme_fira() +
  facet_wrap(~Driver) +
  labs(x = "Season", y = "Skill (log odds ratio)", title = "F1 driver skill trajectories",
       subtitle = "Hybrid-era (2014-2021) driver skill,\naccounting for yearly constructor advantage.")


ggsave("img/plt_skill_trajectories.png", plot = plt_skill_trajectory, width = 12, height = 9, bg = "white")

drivers_2021 <-
  f1_dat %>%
  filter(finished, year == 2021) %>%
  pull(driver) %>%
  unique() %>%
  as.character()

plt_driver_skill_2021 <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Year == 2021, Driver %in% drivers_2021) %>%
  mutate(Driver = fct_reorder(Driver, est),
         Driver = fct_recode(Driver, verstappen = "max_verstappen", schumacher = "mick_schumacher"),
         Driver = fct_relabel(Driver, str_to_title)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = firaCols[3]) +
  theme_fira() +
  labs(title = "2021 F1 driver skill",
       subtitle = "Accounting for yearly constructor advantage.",
       x = "Skill (log odds ratio)",
       y = "Driver")

ggsave("img/plt_skill_2021.png", plot = plt_driver_skill_2021, width = 9, height = 9, bg = "white")


# Inference about constructor advantage ----
recode_constructors <- c(
  "Alfa Romeo" = "alfa",
  "Alpha Tauri" = "alphatauri",
  "Alpine" = "alpine",
  "Ferrari" = "ferrari",
  "Haas" = "haas",
  "McLaren" = "mclaren",
  "Mercedes" = "mercedes",
  "Aston Martin" = "aston_martin",
  "Red Bull" = "red_bull",
  "Williams" = "williams"
)

constructors_focus <- c("ferrari", "haas", "mclaren", "mercedes", "red_bull", "williams")

constructor_mean <- fit$draws("theta_team", format = "df") %>% select(-.chain, -.iteration)
constructor_form <- fit$draws("theta_team_season", format = "df") %>% select(-.chain,-.iteration)

first_year_per_constructor <-
  f1_dat %>%
  group_by(constructor) %>%
  summarize(first_year = first(year))

constructor_mean_long <-
  constructor_mean  %>%
  pivot_longer(-.draw, names_to = "constructor_id", values_to = "Advantage",
               names_pattern = "\\[(\\d+)]", names_transform = as.integer) %>%
  mutate(Constructor = as_factor(levels(f1_dat$constructor)[constructor_id]))

constructor_form_long <-
  constructor_form %>%
  pivot_longer(-.draw, names_to = c("constructor_id", "season_num"), values_to = "Form",
               names_pattern = "\\[(\\d+),(\\d+)\\]", names_transform = as.integer) %>%
  mutate(Constructor = as_factor(levels(f1_dat$constructor)[constructor_id]),
         Year = season_num + 2013)

constructor_samples <-
  left_join(constructor_form_long, constructor_mean_long, by = c("Constructor", ".draw")) %>%
  mutate(advantage_yr = Advantage + Form)

constructor_advantage_summary <-
  constructor_samples %>%
  group_by(Constructor, Year) %>%
  summarise(
    est = mean(advantage_yr),
    lower = quantile(advantage_yr, 0.055),
    upper = quantile(advantage_yr, 0.945),
  ) %>%
  left_join(first_year_per_constructor, by = c("Constructor" = "constructor")) %>%
  filter(Year >= first_year) %>%
  select(-first_year)


plt_advantage_trajectory <-
  constructor_advantage_summary %>%
  ungroup() %>%
  filter(Constructor %in% constructors_focus) %>%
  mutate(Constructor = fct_relevel(Constructor, "ferrari", "mercedes", "red_bull", "mclaren", "haas", "williams"),
         Constructor = fct_recode(Constructor, !!!recode_constructors)) %>%
  ggplot(aes(x = Year, y = est, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(fill = Constructor), alpha = .2) +
  geom_line(aes(colour = Constructor)) +
  geom_point(aes(colour = Constructor)) +
  scale_fill_fira(guide = "none") +
  scale_colour_fira(guide = "none") +
  theme_fira() +
  facet_wrap(~Constructor) +
  labs(x = "Season", y = "Advantage (log odds ratio)", title = "F1 constructor advantage trajectories",
       subtitle = "Hybrid-era (2014-2021) constructor advantage,\naccounting for yearly driver skill.")

ggsave("img/plt_advantage_trajectory.png", plot = plt_advantage_trajectory, width = 12, height = 9, bg = "white")


constructors_2021 <- c("alfa", "alphatauri", "alpine", "ferrari", "haas", "mclaren",
                       "mercedes", "aston_martin", "red_bull", "williams")


constructor_mean_summary <-
  constructor_mean_long %>%
  group_by(Constructor) %>%
  summarise(
    est = mean(Advantage),
    lower = quantile(Advantage, 0.055),
    upper = quantile(Advantage, 0.945),
  )

plt_advantage_avg <-
  constructor_mean_summary %>%
  ungroup() %>%
  filter(Constructor %in% constructors_2021) %>%
  mutate(Constructor = fct_reorder(Constructor, est),
         Constructor = fct_recode(Constructor, !!!recode_constructors)) %>%
  ggplot(aes(y = Constructor, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = firaCols[1]) +
  theme_fira() +
  labs(title = "Average hybrid-era F1 constructor advantage",
       subtitle = "Accounting for yearly driver skill and constructor form.",
       x = "Advantage (log odds ratio)",
       y = "Constructor")

ggsave("img/plt_advantage_avg.png", plot = plt_advantage_avg, width = 9, height = 6, bg = "white")



# Driver versus constructor contributions ----
# random effects standard deviation summary
sd_driver <- fit$draws("tau_driver")
sd_driver_season <- fit$draws("tau_driver_season")
sd_team <- fit$draws("tau_team")
sd_team_season <- fit$draws("tau_team_season")

smry <- function(x) c(mean(x), sd(x), quantile(x, c(0.045, 0.955)))
ranef_summary <- rbind(
  "constructor"      = smry(sd_team),
  "constructor form" = smry(sd_team_season),
  "driver"           = smry(sd_driver),
  "driver form"      = smry(sd_driver_season)
)
colnames(ranef_summary) <- c("Estimate", "Est.Error", "Lower", "Upper")
xtable::xtable(ranef_summary)

# variances
var_driver <- rank_sd_driver^2 + rank_sd_driver_season^2
var_team <- rank_sd_team^2 + rank_sd_team_season^2

# how much of variance is due to car?
prop_team <- var_team / (var_team + var_driver)
smry(prop_team)

# and how much due to the driver?
prop_driver <- var_driver / (var_team + var_driver)
smry(prop_driver)

