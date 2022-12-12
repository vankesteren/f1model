# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: MCMC validation, posterior predictive checks
library(tidyverse)
library(bayesplot)
library(posterior)
library(firatheme)

f1_dat <- read_rds("dat/f1_dat.rds") %>% filter(finished)
fit <- read_rds("fit/basic.rds")

# MCMC mixing ----
mcmc_trace(fit$draws(c("tau_driver", "tau_driver_season", "tau_team", "tau_team_season"))) +
  theme_fira() +
  scale_colour_fira(guide = "none")
ggsave("img/chains.png", width = 7, height = 10, bg = "white")

# Rhat ----
rhats <- rhat(fit)
any(rhats[!is.nan(rhats)] > 1.01)

# 2019 posterior predictive check ----
# we need to be able to sample from gumbel distribution, see
# http://www.glicko.net/research/multicompetitor.pdf , equation 2
rgumbel <- function(n, theta) {
  sapply(theta, function(th) th-log(-log(runif(n))))
}

# create drivers & constructors in 2019
ordered_levels <-
  f1_dat %>%
  filter(year == 2019) %>%
  select(driver, constructor, year, position) %>%
  group_by(driver) %>%
  summarise(pos = mean(position)) %>%
  arrange(pos) %>%
  pull(driver) %>%
  as.character()

pred_tab <-
  f1_dat %>%
  filter(year == 2019) %>%
  select(driver, constructor, year) %>%
  distinct()

idx_driver <- pred_tab$driver |> as.integer()
idx_team <- pred_tab$constructor |> as.integer()

driver_skill <- fit$draws("theta_driver")
driver_form <- subset_draws(fit$draws("theta_driver_season"), "theta_driver_season\\[\\d+\\,6\\]", regex = TRUE)
#circuit_effect <- fit$draws("circuit_effect")
team_skill <- fit$draws("theta_team")
team_form <- subset_draws(fit$draws("theta_team_season"), "theta_team_season\\[\\d+\\,6\\]", regex = TRUE)


latent_skill <-
  as_draws_df(
    driver_skill[,,idx_driver] +
      driver_form[,,idx_driver] +
     # circuit_effect[,,idx_driver] +
      team_skill[,,idx_team] +
      team_form[,,idx_team]
  ) |>
  pivot_longer(
    starts_with("theta_driver"),
    names_to = c("driver_id"),
    names_pattern = "theta_driver\\[(\\d+)]",
    names_transform = as.integer
  ) |>
  mutate(driver_name = levels(f1_dat |> pull(driver))[driver_id]) |>
  select(.draw, driver_name, value) |>
  mutate(performance = rgumbel(1, value))


# TODO: make pp check on a per-race basis, not all races have 20 competitors!
yrep <-
  latent_skill |>
  group_by(.draw) |>
  summarize(driver_name = driver_name, .draw = .draw, position = rank(-performance)) |>
  ungroup() |>
  mutate(origin = "simulated")

y <-
  f1_dat %>%
  filter(year == 2019) %>%
  select(driver_name = driver, position) %>%
  mutate(origin = "observed")

bind_rows(y, yrep) |>
  ggplot(aes(x = factor(position), fill = origin, group = origin)) +
  geom_bar(aes(y = after_stat(prop)), position = position_dodge(preserve = "single")) +
  facet_wrap(~factor(driver_name, levels = ordered_levels)) +
  theme_linedraw() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 70, vjust = 1)) +
  labs(
    title = "2019 season posterior predictive check",
    x = "Position",
    y = "",
    fill = ""
  )

ggsave("img/pp_check_rank_2019.png", width = 15, height = 12, bg = "white")



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
