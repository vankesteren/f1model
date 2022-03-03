# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Inferences using posteriors of parameters
library(tidyverse)
library(brms)
library(firatheme)
library(patchwork)
library(glue)

fit <- read_rds("fit/fit_basic.rds")

# Inference about driver skill ----
drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

driver_mean <- as_draws_df(fit, variable = "r_driver\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
driver_form <- as_draws_df(fit, variable = "r_driver:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)


driver_mean_long <-
  driver_mean  %>%
  pivot_longer(-.draw, names_to = "Driver", values_to = "Skill",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Driver = as_factor(Driver))

driver_form_long <-
  driver_form %>%
  pivot_longer(-.draw, names_to = c("Driver", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Driver = as_factor(Driver), Year = as.integer(Year))

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
  )


plt_skill_trajectory <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Driver %in% drivers_focus) %>%
  mutate(Driver = fct_reorder(Driver, -est)) %>%
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

plt_driver_skill_2021 <-
  driver_skill_summary %>%
  ungroup() %>%
  filter(Year == 2021) %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = firaCols[3]) +
  theme_fira() +
  labs(title = "2021 F1 driver skill",
       subtitle = "Accounting for yearly constructor advantage.",
       x = "Skill (log odds ratio)",
       y = "Driver")

ggsave("img/plt_skill_2021.png", plot = plt_driver_skill_2021, width = 9, height = 9, bg = "white")


# Inference about constructor advantage ----
constructors_focus <- c("ferrari", "haas", "mclaren", "mercedes", "red_bull", "williams")

constructor_mean <- as_draws_df(fit, variable = "r_constructor\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
constructor_form <- as_draws_df(fit, variable = "r_constructor:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)


constructor_mean_long <-
  constructor_mean  %>%
  pivot_longer(-.draw, names_to = "Constructor", values_to = "Advantage",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Constructor = as_factor(Constructor))

constructor_form_long <-
  constructor_form %>%
  pivot_longer(-.draw, names_to = c("Constructor", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Constructor = as_factor(Constructor), Year = as.integer(Year))

constructor_samples <-
  left_join(constructor_form_long, constructor_mean_long, by = c("Constructor", ".draw")) %>%
  mutate(advantage_yr = Form + Advantage)

constructor_advantage_summary <-
  constructor_samples %>%
  group_by(Constructor, Year) %>%
  summarise(
    est = mean(advantage_yr),
    lower = quantile(advantage_yr, 0.055),
    upper = quantile(advantage_yr, 0.945),
  )

plt_advantage_trajectory <-
  constructor_advantage_summary %>%
  ungroup() %>%
  filter(Constructor %in% constructors_focus) %>%
  mutate(Constructor = fct_relevel(Constructor, "ferrari", "mercedes", "red_bull", "mclaren", "haas", "williams")) %>%
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
  mutate(Constructor = fct_reorder(Constructor, est)) %>%
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
sfit <- summary(fit, prob = 0.89)
ranef_summary <- rbind(
  "constructor" = sfit$random$constructor,
  "constructor form" = sfit$random$`constructor:year`,
  "driver" = sfit$random$driver,
  "driver form" = sfit$random$`driver:year`
)[1:4, 1:4]
xtable::xtable(ranef_summary)

# how much of variance is due to car?
colSums(ranef_summary[1:2,]^2)/colSums(ranef_summary^2)

# and how much due to the driver?
colSums(ranef_summary[3:4,]^2)/colSums(ranef_summary^2)



# Overall performance in 2021 ----
grid_2021 <-
  f1_dat %>%
  filter(year == 2021, driver != "kubica") %>% # kubica only did one race
  select(driver, constructor, year) %>%
  distinct() %>%
  arrange(constructor)

pp_2021 <- posterior_predict(fit, grid_2021)
pp_2021_summary <-
  pp_2021 %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(grid_2021$driver) %>%
  pivot_longer(everything(), names_to = "driver") %>%
  group_by(driver) %>%
  summarise(est = mean(value), lower = quantile(value, 0.045), upper = quantile(value, 0.955)) %>%
  left_join(grid_2021) %>%
  select(driver, constructor, performance = est, lower, upper) %>%
  arrange(-performance)

xtable::xtable(pp_2021_summary, digits = 3)

ggsave("img/plt_performance_2021.png", width = 6, height = 9, bg = "white")
