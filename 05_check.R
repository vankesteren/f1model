# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: MCMC validation, posterior predictive checks
library(tidyverse)
library(brms)
library(firatheme)

f1_dat <- read_rds("dat/f1_dat_finished.rds")
fit <- read_rds("fit/fit_basic.rds")

# MCMC mixing ----
mcmc_plot(fit, type = "trace") +
  facet_wrap(~parameter, nrow = 6, scales = "free") +
  theme_fira() +
  scale_colour_fira()
ggsave("img/chains.png", width = 7, height = 10, bg = "white")

# Rhat ----
rhats <- rhat(fit)
any(rhats[!is.nan(rhats)] > 1.01)

# 2019 posterior predictive check ----
# create drivers & constructors in 2019
pred_tab <-
  f1_dat %>%
  filter(year == 2019) %>%
  select(driver, constructor, year) %>%
  distinct() %>%
  mutate(weather_type = "dry", circuit_type = "permanent")

# predict proportion of outperformed drivers
pp_tab <- posterior_predict(fit, pred_tab)


## Proportion plot ----
# yrep
pred_tab_long <-
  pred_tab %>%
  bind_cols(t(pp_tab) %>% as_tibble(.name_repair = "minimal") %>% set_names(1:10000)) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "prop_trans"
  ) %>%
  mutate(origin = "simulated")

# y
true_tab_long <-
  f1_dat %>%
  filter(year == 2019) %>%
  select(driver, constructor, year, prop_trans) %>%
  mutate(origin = "observed")

ordered_levels <-
  true_tab_long %>%
  group_by(driver) %>%
  summarise(prop = mean(prop_trans)) %>%
  arrange(-prop) %>%
  pull(driver) %>%
  as.character()


bind_rows(pred_tab_long, true_tab_long) %>%
  ggplot(aes(x = prop_trans, fill = origin)) +
  geom_density(alpha = 0.8, bw = .07) +
  facet_wrap(~factor(driver, levels = ordered_levels), scales = "free") +
  xlim(0, 1) +
  theme_fira() +
  scale_fill_fira() +
  theme(legend.position = "top") +
  labs(
    title = "2019 season posterior predictive check",
    x = "Proportion of outperformed drivers",
    y = "",
    fill = ""
  )

ggsave("img/pp_check_prop_2019.png", width = 15, height = 12, bg = "white")



## PPC on rank scale ----

# finish position distribution to weigh observations by
n_races <- length(unique(paste0(f1_dat$year, "_", f1_dat$round)))
position_table <- table(f1_dat$position) / n_races
reweigh <- function(rank_sample) {
  # function to resample the ranks based on their value
  sample(rank_sample, prob = position_table[rank_sample], replace = TRUE)
}

pp_ranks <-
  apply(pp_tab, 1, function(x) rank(-x)) %>%
  apply(1, reweigh) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(across(.fns = as.integer)) %>%
  set_names(1:10000)



# yrep
pred_rank_long <-
  pred_tab %>%
  bind_cols(pp_ranks) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "position"
  ) %>%
  mutate(origin = "simulated")

# y
true_rank_long <-
  f1_dat %>%
  filter(year == 2019) %>%
  select(driver, constructor, year, position) %>%
  mutate(origin = "observed")


bind_rows(pred_rank_long, true_rank_long) %>%
  filter(is.na(sample) | sample %in% sample(10000, 23)) %>%
  ggplot(aes(x = factor(position), fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  facet_wrap(~factor(driver, levels = ordered_levels)) +
  theme_fira() +
  scale_fill_fira() +
  theme(legend.position = "top") +
  labs(
    title = "2019 season posterior predictive check",
    x = "Position",
    y = "",
    fill = ""
  )

ggsave("img/pp_check_rank_2019.png", width = 15, height = 12, bg = "white")


# 2015 posterior predictive check ----
# create drivers & constructors in 2015
pred_tab <-
  f1_dat %>%
  filter(year == 2015) %>%
  select(driver, constructor, year) %>%
  distinct() %>%
  mutate(weather_type = "dry", circuit_type = "permanent")

# predict proportion of defeated drivers
pp_tab <- posterior_predict(fit, pred_tab)


## Proportion plot ----
# yrep
pred_tab_long <-
  pred_tab %>%
  bind_cols(t(pp_tab) %>% as_tibble() %>% set_names(1:10000)) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "prop_trans"
  ) %>%
  mutate(origin = "simulated")

# y
true_tab_long <-
  f1_dat %>%
  filter(year == 2015) %>%
  select(driver, constructor, year, prop_trans) %>%
  mutate(origin = "observed")

ordered_levels <-
  true_tab_long %>%
  group_by(driver) %>%
  summarise(prop = mean(prop_trans)) %>%
  arrange(-prop) %>%
  pull(driver) %>%
  as.character()


bind_rows(pred_tab_long, true_tab_long) %>%
  ggplot(aes(x = prop_trans, fill = origin)) +
  geom_density(alpha = 0.8, bw = .07) +
  facet_wrap(~factor(driver, levels = ordered_levels), scales = "free") +
  xlim(0, 1) +
  theme_fira() +
  scale_fill_fira() +
  theme(legend.position = "top") +
  labs(
    title = "2015 season posterior predictive check",
    x = "Proportion of outperformed drivers",
    y = "",
    fill = ""
  )

ggsave("img/pp_check_prop_2015.png", width = 15, height = 12, bg = "white")



## PPC on rank scale ----
pp_ranks <-
  apply(pp_tab, 1, function(x) rank(-x)) %>%
  apply(1, reweigh) %>%
  t() %>%
  as_tibble() %>%
  mutate(across(.fns = as.integer)) %>%
  set_names(1:10000)

# yrep
pred_rank_long <-
  pred_tab %>%
  bind_cols(pp_ranks) %>%
  pivot_longer(
    cols      = c(-driver, -constructor, -year, -weather_type, -circuit_type),
    names_to  = "sample",
    values_to = "position"
  ) %>%
  mutate(origin = "simulated")

# y
true_rank_long <-
  f1_dat %>%
  filter(year == 2015) %>%
  select(driver, constructor, year, position) %>%
  mutate(origin = "observed")


bind_rows(pred_rank_long, true_rank_long) %>%
  filter(is.na(sample) | sample %in% sample(10000, 23)) %>%
  ggplot(aes(x = factor(position), fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  facet_wrap(~factor(driver, levels = ordered_levels)) +
  theme_fira() +
  scale_fill_fira() +
  theme(legend.position = "top") +
  labs(
    title = "2015 season posterior predictive check",
    x = "Position",
    y = "",
    fill = ""
  )

ggsave("img/pp_check_rank_2015.png", width = 15, height = 12, bg = "white")
