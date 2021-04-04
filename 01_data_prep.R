library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

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

f1_dat <- tibble(
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
    cat(glue("Downloading race {rnd} of season {yr}..."))
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
    f1_dat <- bind_rows(f1_dat, dat)
    cat(" Done.\n")
  }
}

write_rds(f1_dat, "dat/f1_dat.rds")

