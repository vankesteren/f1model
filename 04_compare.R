# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Performing model comparison
library(tidyverse)
library(brms)
library(xtable)

options(mc.cores = 12)

# which model is best? Compare using LOO
fit_basic   <- read_rds("fit/fit_basic.rds") %>% add_criterion("loo")
fit_circuit <- read_rds("fit/fit_circuit.rds") %>% add_criterion("loo")
fit_weather <- read_rds("fit/fit_weather.rds") %>% add_criterion("loo")
fit_all     <- read_rds("fit/fit_weather_circuit.rds") %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic,
  fit_circuit,
  fit_weather,
  fit_all,
  model_names = c("Basic", "Circuit", "Weather", "Circuit + Weather")
)

loo_results

write_rds(loo_results, "fit/loo_results.rds")


xtable::xtable(loo_results)

#                   elpd_diff se_diff
# Weather            0.0       0.0
# Basic             -0.3       2.2
# Circuit + Weather -0.7       0.6
# Circuit           -2.3       2.3

# model including weather works best, but is similar to basic model
elpd_diff  se_diff    elpd_loo   se_elpd_loo p_loo      se_p_loo   looic      se_looic
<compar.l> <compar.l> <compar.l> <compar.l>  <compar.l> <compar.l> <compar.l> <compar.l>
1  0.0000000 0.0000000  1443.505   46.58635    120.9712   4.459578   -2887.010  93.17271
2 -0.3050344 1.4172372  1443.200   46.61676    127.8621   4.792843   -2886.400  93.23351
3 -0.3836410 0.5706045  1443.122   46.54169    123.3310   4.554304   -2886.243  93.08338
4 -2.4080530 1.4606067  1441.097   46.61889    131.6624   4.928787   -2882.194  93.23779
