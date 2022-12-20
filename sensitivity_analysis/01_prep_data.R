# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2021-05-16 by @vankesteren
# Contents: Create different datasets for sensitivity analysis

# load packages
library(tidyverse)

# read data
f1_dat <-
  read_rds("dat/f1_dat.rds")

# create function for different finishing indicators
compute_classified <- function(status, exp) {
  out <- rep(FALSE, length(status))
  last_classified <- which(eval(exp))
  out[last_classified] <- TRUE
  out
}

# different expressions to indicate who classified as finishing + position
# 1. = only drivers who truly finished are classified
# 2. = 1 + anyone above the last person still running (finished or +n laps is classified)
# 3. = 2 + drivers who had an accident or collision
exp <- list(exp1 = expr(status == "Finished"),
            exp2 = expr(status == "Finished" | str_starts(status, "\\+")),
            exp3 = expr(status == "Finished" | str_starts(status, "\\+") | str_starts(status, "Acc|Coll")))

for(i in 1:length(exp)){

  temp <- f1_dat %>%
    group_by(year, round) %>%
    mutate(finished = compute_classified(status, exp = exp[[i]])) %>%
    # reset finishing position, e.g. collisions are 'finished' higher than car breakdowns
    arrange(desc(finished), position, .by_group = T) %>%
    mutate(position= 1:n()) %>%
    ungroup()

  assign(paste0("f1_dat_finished_", i), temp)

  # save data
  write_rds(temp, paste0("sensitivity_analysis/data/f1_dat_finished_", i, ".rds"))
}





