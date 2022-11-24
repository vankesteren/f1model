library(tidyverse)
library(cmdstanr)
library(ggrepel)

# load data
f1_dat <-
  read_rds("dat/f1_dat.rds") |>
  mutate(
    status       = as_factor(status),
    constructor  = as_factor(constructor),
    driver       = as_factor(driver),
    weather_type = as_factor(weather_type),
    circuit_type = as_factor(circuit_type)
  ) |>
  filter() |>
  mutate(across(where(is.factor), fct_drop))

# Compute classified indicator
# exclude all collisions & non-finishes
compute_classified <- function(status) {
  out <- rep(FALSE, length(status))
  # anyone above the last person still running (finished or +n laps is classified)
  last_classified <- max(which(status == "Finished" | str_starts(status, "\\+")))
  out[1:last_classified] <- TRUE
  out
}

f1_dat <-
  f1_dat |>
  group_by(year, round) |>
  mutate(finished = compute_classified(status),
         position = ifelse(finished, position, NA))

# Basic ROL model ----
rol_mod <- cmdstan_model("stan_models/rank_ordered_logit.stan")
stan_data <- list(
  M = f1_dat |> filter(finished) |> pull(driver) |> nlevels(),
  K = f1_dat |> group_by(year, round) |> n_groups(),
  N = f1_dat |> filter(finished) |> nrow(),
  ranked_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(driver) |> as.integer(),
  num_competitors = f1_dat |> filter(finished) |> group_by(year, round) |> summarize(count = n()) |> pull(count)
)

# Optimize
rol_opt <- rol_mod$optimize(data = stan_data)
rol_opt$summary(variables = "theta") |>
  mutate(ID = levels(f1_dat |> filter(finished) |> pull(driver))[row_number()]) |>
  arrange(-estimate)

# Sample
rol_fit <- rol_mod$sample(data = stan_data, parallel_chains = 4)
rol_fit$summary(variables = "theta") |>
  mutate(ID = levels(f1_dat |> filter(finished) |> pull(driver))[row_number()]) |>
  arrange(-mean)


# Driver-constructor model ----
roldc_mod <- cmdstan_model("stan_models/rank_ordered_logit_driver_constructor.stan")
stan_data <- list(
  M_d = f1_dat |> filter(finished) |> pull(driver) |> nlevels(),
  M_c = f1_dat |> filter(finished) |> pull(constructor) |> nlevels(),
  K = f1_dat |> group_by(year, round) |> n_groups(),
  N =  f1_dat |> filter(finished) |> nrow(),
  ranked_driver_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(driver) |> as.integer(),
  ranked_constructor_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(constructor) |> as.integer(),
  num_entrants = f1_dat |> filter(finished) |> group_by(year, round) |> summarize(count = n()) |> pull(count)
)

# Optimize
roldc_opt <- roldc_mod$optimize(data = stan_data)

# driver skills
roldc_opt$summary(variables = "theta_driver") |>
  mutate(ID = levels(f1_dat |> filter(finished) |> pull(driver))[row_number()]) |>
  arrange(-estimate)

# constructor contributions
roldc_opt$summary(variables = "theta_constructor") |>
  mutate(ID = levels(f1_dat |> filter(finished) |> pull(constructor))[row_number()]) |>
  arrange(-estimate)

# proportion of variance explained
var_driver <- roldc_opt$summary("sigma_driver")$estimate^2
var_constructor <- roldc_opt$summary("sigma_constructor")$estimate^2
var_constructor / (var_driver + var_constructor)

# Sample
roldc_fit <- roldc_mod$sample(data = stan_data, parallel_chains = 4, iter_sampling = 5000)

# driver skills
roldc_fit$summary(variables = "theta_driver") |>
  mutate(ID = levels(f1_dat |> filter(finished) |> pull(driver))[row_number()]) |>
  arrange(-mean)

# constructor contributions
roldc_fit$summary(variables = "theta_constructor") |>
  mutate(ID = levels(f1_dat |> filter(finished) |> pull(constructor))[row_number()]) |>
  arrange(-mean)

# proportion of variance explained
var_driver <- roldc_fit$summary("sigma_driver")$mean^2
var_constructor <- roldc_fit$summary("sigma_constructor")$mean^2
var_constructor / (var_driver + var_constructor)

# Driver-constructor model with innovations
roldci_mod <- cmdstan_model("stan_models/rank_ordered_logit_driver_constructor_innovation.stan")
stan_data <- list(
  M_d = f1_dat |> filter(finished) |> pull(driver) |> nlevels(),
  M_c = f1_dat |> filter(finished) |> pull(constructor) |> nlevels(),
  K = f1_dat |> group_by(year, round) |> n_groups(),
  N =  f1_dat |> filter(finished) |> nrow(),
  S = f1_dat |> filter(finished) |> group_by(year) |> n_groups(),
  ranked_driver_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(driver) |> as.integer(),
  ranked_constructor_ids = f1_dat |> filter(finished) |> arrange(year, round, position) |> pull(constructor) |> as.integer(),
  num_entrants = f1_dat |> filter(finished) |> group_by(year, round) |> summarize(count = n()) |> pull(count),
  season_id = f1_dat |> filter(finished) |> group_by(year, round) |> summarize(year = factor(first(year))) |> pull(year) |> as.integer()
)

# optim
roldci_opt <- roldci_mod$optimize(stan_data, iter = 10000)

skill_df <-
  roldci_opt$summary("theta_driver") |>
  mutate(
    driver_id = as.numeric(str_extract(variable, "\\d+(?=,)")),
    race_number = as.numeric(str_extract(variable, "\\d+(?=\\])")),
    driver_name = levels(f1_dat |> pull(driver))[driver_id])


drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

skill_df |>
  filter(driver_name %in% drivers_focus) |>
  ggplot(aes(x = race_number, y = estimate, colour = driver_name)) +
  geom_line() +
  theme_minimal()


# sample from the posterior (TAKES A LONG TIME!)
roldci_fit <- roldci_mod$sample(stan_data, chains = 4, parallel_chains = 4, iter_warmup = 500, iter_sampling = 1000)
roldci_fit$save_object("stan_models/fits/roldci_fit.rds")

skill_df <-
  roldci_fit$summary("theta_driver") |>
  mutate(
    driver_id = as.numeric(str_extract(variable, "\\d+(?=,)")),
    season_number = as.numeric(str_extract(variable, "\\d+(?=\\])")),
    driver_name = levels(f1_dat |> pull(driver))[driver_id]
  )

drivers_focus <- c("hamilton", "bottas", "norris", "sainz", "leclerc", "max_verstappen", "perez", "alonso",
                   "raikkonen", "giovinazzi", "vettel", "gasly")

skill_df |>
  #filter(driver_name %in% drivers_focus) |>
  mutate(label = if_else(season_number == max(season_number), as.character(driver_name), NA_character_)) |>
  ggplot(aes(x = 2013 + season_number, y = mean, colour = driver_name, fill = driver_name, ymin = mean - sd, ymax = mean + sd, label = label)) +
  #geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line() +
  geom_label_repel(colour = "black", fill = "white", na.rm = TRUE) +
  theme_minimal() +
  scale_color_viridis_d(guide = "none") +
  scale_fill_viridis_d(guide = "none") +
  scale_x_continuous(breaks = 2014:2021) +
  theme(legend.position = "none") +
  labs(x = "Season", y = "Skill")

contrib_df <-
  roldci_fit$summary("theta_constructor") |>
  mutate(
    constructor_id = as.numeric(str_extract(variable, "\\d+(?=,)")),
    season_number = as.numeric(str_extract(variable, "\\d+(?=\\])")),
    constructor_name = levels(f1_dat |> pull(constructor))[constructor_id]
  )

constructors_focus <- c("mercedes", "red_bull", "ferrari", "williams", "mclaren", "toro_rosso")

contrib_df |>
  filter(constructor_name %in% constructors_focus) |>
  mutate(label = if_else(season_number == max(season_number), as.character(constructor_name), NA_character_)) |>
  ggplot(aes(x = 2013 + season_number, y = mean, colour = constructor_name, fill = constructor_name,
             ymin = mean - sd, ymax = mean + sd, label = label)) +
  geom_ribbon(alpha = 0.2, colour = NA) +
  geom_line() +
  geom_label_repel(colour = "black", fill = "white", na.rm = TRUE) +
  theme_minimal() +
  scale_color_viridis_d(guide = "none") +
  scale_fill_viridis_d(guide = "none") +
  scale_x_continuous(breaks = 2014:2021) +
  theme(legend.position = "none") +
  labs(x = "Season", y = "Contribution")
