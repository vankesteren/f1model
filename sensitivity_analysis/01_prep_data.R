# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2023-02-02 by @vankesteren
# Contents: Create different datasets for sensitivity analysis

# load packages
library(tidyverse)

# read data
f1_dat <-  read_rds("dat/f1_dat.rds")

# different expressions to indicate who classified as finishing + position
# 1. = Not removing any results, i.e., ranking unfinished competitors based on race distance
# 2. = Removing only car-related non-finishes and ranking the driver-related non-finishes as normal
# 3. = Removing only driver-related non-finishes and ranking the car-related non-finishes as normal
# 4. = only ranking finished competitiors (i.e orignal dataset)

driver_keywords <- c("Collision", "Disqualified", "Withdrew", "Retired", "Accident",
                   "Collision damage", "Spun off", "Excluded", "Illness")

team_keywords <- c("ERS", "Oil pressure", "Engine", "Technical", "Gearbox",
                   "Electrical", "Power Unit", "Brakes", "Clutch", "Exhaust", "Mechanical",
                   "Turbo", "Rear wing", "Drivetrain", "Suspension", "Oil leak",
                   "Water leak", "Water pressure", "Electronics", "Transmission",
                   "Wheel", "Power loss", "Fuel system", "Front wing", "Tyre", "Throttle",
                   "Brake duct", "Hydraulics", "Battery", "Puncture", "Overheating",
                   "Wheel nut", "Vibrations", "Driveshaft", "Fuel pressure", "Seat",
                   "Spark plugs", "Steering", "Damage", "Out of fuel", "Debris", "Radiator")

f1_dat <-
  f1_dat %>%
  mutate(
    driver_error = status %in% driver_keywords,
    team_error = status %in% team_keywords
  )

# all results
f1_dat %>%
  select(-status, -finished, -driver_error, -team_error) %>%
  write_rds(file = "sensitivity_analysis/data/f1_dat_all.rds")

# exclude car-related errors
f1_dat %>%
  filter(finished | driver_error) %>%
  select(-status, -finished, -driver_error, -team_error) %>%
  write_rds(file = "sensitivity_analysis/data/f1_dat_excl_car.rds")

# exclude driver-related errors
f1_dat %>%
  filter(finished | team_error) %>%
  select(-status, -finished, -driver_error, -team_error) %>%
  write_rds(file = "sensitivity_analysis/data/f1_dat_excl_driver.rds")

# as in main paper
f1_dat %>%
  filter(finished) %>%
  select(-status, -finished, -driver_error, -team_error) %>%
  write_rds(file = "sensitivity_analysis/data/f1_dat_finished.rds")

