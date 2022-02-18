# Code accompanying the manuscript "Bayesian Analysis of Formula One Race Results"
# Last edited 2022-02-17 by @vankesteren
# Contents: data preparation, data joining from database f1db_csv.
library(tidyverse)
library(lubridate)
library(jsonlite)
library(glue)
library(rvest)

# Date range for the analysis
hybrid_era <- interval(ymd("2014-01-01"), ymd("2021-12-31"))

# Race information
tab_races        <- read_csv("dat/f1db_csv/races.csv")
tab_circuits     <- read_csv("dat/f1db_csv/circuits.csv")

# Results information
tab_results      <- read_csv("dat/f1db_csv/results.csv")
tab_drivers      <- read_csv("dat/f1db_csv/drivers.csv")
tab_constructors <- read_csv("dat/f1db_csv/constructors.csv")
tab_status       <- read_csv("dat/f1db_csv/status.csv")

# Race information ----
## function to info from wikipedia ----
wiki_info <- function(url) {
  cat(glue("Downloading info from {url}..."),"\n")

  infobox <- tryCatch({
    read_html(url) %>%
      html_element(".infobox") %>%
      html_table(trim = TRUE) %>%
      .[,1:2] %>%
      set_names(c("Property", "Value"))
  }, error = function(e) NA)

  if (length(infobox) == 1 && is.na(infobox)) return(NA)

  weather_txt <- tryCatch({
    res <-
      infobox %>%
      filter(Property == "Weather") %>%
      pull(Value)
    stopifnot(length(res) > 0)
    res
  }, error = function(e) NA)

  circuit_txt <- tryCatch({
    res <-
      infobox %>%
      filter(Property == "Course") %>%
      pull(Value)
    stopifnot(length(res) > 0)
    res
  }, error = function(e) NA)

  return(list(circuit_txt = circuit_txt, weather_txt = weather_txt))
}

## Create enriched race info dataset ----
race_info <-
  tab_races %>%
  filter(date %within% hybrid_era) %>%
  left_join(tab_circuits, by = "circuitId", suffix = c("", "_circuit")) %>%
  mutate(enrichment = lapply(url, wiki_info)) %>%
  unnest_wider(enrichment) %>%
  mutate(
    weather_type = ifelse(str_detect(tolower(weather_txt), "(wet|rain)"), "wet", "dry"),
    circuit_type = ifelse(str_detect(tolower(circuit_txt), "street"), "street", "permanent"),
  )

## Manually add missing weather info ----
weather_missing_idx <- which(is.na(race_info$weather_type))
race_info[weather_missing_idx, "weather_type"] <- c(
  "dry", # bahrain 2017
  "dry", # sochi 2017
  "dry", # spain 2017
  "dry", # monaco 2017
  "dry", # brazil 2017
  "dry", # bahrain 2018
  "dry", # shanghai 2018
  "dry", # monaco 2018
  "dry", # hungary 2018
  "dry", # belgium 2018
  "dry", # italy 2018
  "dry", # singapore 2018
  "dry", # russia 2018
  "dry", # japan 2018
  "dry", # usa 2018
  "dry", # mexico 2018
  "dry", # brazil 2018
  "dry", # abu dhabi 2018
  "dry", # australia 2019
  "dry", # china 2019
  "dry", # azerbaijan 2019
  "dry", # spain 2019
  "dry"  # brazil 2021
)

## Manually add missing circuit info ----
circuit_missing_idx <- which(is.na(race_info$circuit_type))
race_info[weather_missing_idx, "circuit_type"] <- c(
  "permanent" # Brazil 2021
)


## Select columns ----
race_dat <- race_info %>%
  select(raceId, year, round, circuitRef, country, weather_type, circuit_type)

# Result information ----
results_dat <-
  tab_results %>%
  left_join(tab_drivers, by = "driverId") %>%
  left_join(tab_constructors, by = "constructorId") %>%
  left_join(tab_status, by = "statusId") %>%
  select(raceId, positionText, positionOrder, fastestLapTime, driverRef, constructorRef, status)

# Joining & cleaning ----
f1_dat <-
  race_dat %>%
  left_join(results_dat, by = "raceId") %>%
  select(-raceId) %>%
  rename(circuit = circuitRef, driver = driverRef, constructor = constructorRef,
         position = positionOrder, fastest_lab = fastestLapTime) %>%
  select(driver, constructor, year, round, circuit, position, weather_type, circuit_type, status) %>%
  mutate(year = as.integer(year), round = as.integer(round), position = as.integer(position))

write_rds(f1_dat, "dat/f1_dat.rds")
