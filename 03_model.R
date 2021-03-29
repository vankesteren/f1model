library(tidyverse)
library(brms)
f1_dat <- read_rds("f1_dat_finished.rds")


fit <- brm(
  formula = prop_trans ~ 0 + (1 | driver + constructor / year),
  family  = Beta(),
  data    = f1_dat,
  backend = "cmdstanr",
  file    = "dat/fit",
  cores   = 4,
  threads = 3
)


summary(fit)
write_rds(fit, "dat/fit.rds")
