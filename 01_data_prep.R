library(tidyverse)
library(httr)
library(jsonlite)
library(glue)
library(rvest)


# Main results ----
# function to download result table for specific race
get_result_table <- function(year, round) {
  url <- parse_url("http://ergast.com")
  url$path <- glue("api/f1/{year}/{round}/results.json")
  res <- GET(url)
  res_json <- fromJSON(rawToChar(res$content))
  dat <- res_json[["MRData"]][["RaceTable"]][["Races"]][["Results"]][[1]] # much nesting
  return(as_tibble(dat))
}

# function to get season info
get_season_table <- function(year) {
  url <- parse_url("http://ergast.com")
  url$path <- glue("api/f1/{year}.json")
  res <- GET(url)
  res_json <- fromJSON(rawToChar(res$content))
  dat <- res_json[["MRData"]][["RaceTable"]][["Races"]]
  return(as_tibble(dat))
}


# create dataset
start_year <- 2014
end_year   <- 2020

result_dat <- tibble(
  driver      = character(),
  driver_no   = integer(),
  position    = integer(),
  status      = character(),
  constructor = character(),
  year        = integer(),
  round       = integer()
)

for (yr in start_year:end_year) {
  yr_dat <- get_season_table(yr)
  for (rnd in yr_dat$round) {
    cat(glue("Downloading season {yr} round {rnd}..."),"\n")
    Sys.sleep(runif(1)*.2)
    rnd_dat <- get_result_table(yr, rnd)
    dat <- tibble(
      driver      = rnd_dat$Driver$driverId,
      driver_no   = as.integer(rnd_dat$Driver$permanentNumber),
      position    = as.integer(rnd_dat$position),
      status      = rnd_dat$status,
      constructor = rnd_dat$Constructor$constructorId,
      year        = as.integer(yr),
      round       = as.integer(rnd)
    )
    result_dat <- bind_rows(result_dat, dat)
  }
}

write_rds(result_dat, "dat/result_dat.rds")

# Circuit information ----

# function to get weather report & circuit type from wikipedia
wiki_info <- function(yr_dat) {
  weather <- character(nrow(yr_dat))
  course  <- character(nrow(yr_dat))
  for (i in 1:nrow(yr_dat)) {
    cat(glue("Downloading info for round {i}..."),"\n")
    Sys.sleep(runif(1)*.2)
    weather[i] <- tryCatch({
      res <-
        read_html(yr_dat$url[i]) %>%
        html_element(".infobox") %>%
        html_table(trim = TRUE) %>%
        .[,1:2] %>%
        set_names(c("Property", "Value")) %>%
        filter(Property == "Weather") %>%
        pull(Value)
      stopifnot(length(res) > 0)
      res
    }, error = function(e) NA)

    course[i] <- tryCatch({
      res <-
        read_html(yr_dat$url[i]) %>%
        html_element(".infobox") %>%
        html_table(trim = TRUE) %>%
        .[,1:2] %>%
        set_names(c("Property", "Value")) %>%
        filter(Property == "Course") %>%
        pull(Value)
      stopifnot(length(res) > 0)
      res
    }, error = function(e) NA)
  }
  return(yr_dat %>% mutate(weather = weather, course = course))
}

circuit_dat <- tibble(
  year = integer(),
  round = integer(),
  circuit = character(),
  country = character(),
  weather = character(),
  circuit_type = character()
)

for (yr in start_year:end_year) {
  cat(glue("### {yr} ###"),"\n")
  yr_dat <-
    get_season_table(yr) %>%
    wiki_info()

  yr_dat <-
    yr_dat %>%
    select(
      year = season,
      round = round,
      weather = weather,
      circuit_type = course
    ) %>%
    mutate(
      year = as.integer(year),
      round = as.integer(round),
      circuit = yr_dat %>% pull(Circuit) %>% pull(circuitId),
      country = yr_dat %>% pull(Circuit) %>% pull(Location) %>% pull(country)
    )
  circuit_dat <- bind_rows(circuit_dat, yr_dat)
}

# post-process and save

circuit_dat <-
  circuit_dat %>%
  mutate(
    weather_full = weather,
    circuit_full = circuit_type,
    weather_type = ifelse(str_detect(tolower(weather), "(wet|rain)"), "wet", "dry"),
    circuit_type = ifelse(str_detect(tolower(circuit_type), "street"), "street", "permanent"),
  )

write_rds(circuit_dat, "dat/circuit_dat.rds")

# Combine everything ----
f1_dat <- left_join(
  result_dat,
  circuit_dat %>% select(year, round, circuit, weather_type, circuit_type),
  by = c("year", "round")
)

write_rds(f1_dat, "dat/f1_dat.rds")

