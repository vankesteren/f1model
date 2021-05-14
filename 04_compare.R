# Model comparison using LOO and WAIC
library(tidyverse)
library(brms)

options(mc.cores = 10)

# which model is best? Compare using LOO
fit_finished <- read_rds("dat/fit_finished.rds") %>% add_criterion("loo")
fit_circuit  <- read_rds("dat/fit_circuit.rds")  %>% add_criterion("loo")
fit_weather  <- read_rds("dat/fit_weather.rds")  %>% add_criterion("loo")
fit_all      <- read_rds("dat/fit_all.rds")      %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_finished,
  fit_circuit,
  fit_weather,
  fit_all,
  model_names = c("Basic", "Circuit", "Weather", "Circuit + Weather")
)

loo_results

write_rds(loo_

#               elpd_diff se_diff
# fit_weather   0.0       0.0
# fit_finished -0.8       2.4
# fit_all      -1.4       0.6
# fit_circuit  -2.3       2.4

# model including weather works best, but is similar to fit_finished.
