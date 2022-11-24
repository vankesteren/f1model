# test rank-ordered logit model in stan
library(tidyverse)
library(cmdstanr)
library(PlackettLuce)

# Compile the Rank-Ordered Logit model
mod_rol <- cmdstan_model("stan_models/rank_ordered_logit.stan")

# Example race data with 4 competitors in 6 races
# Includes NA
R <- matrix(c(
  1,  3,  2,  4,
  2, NA,  1,  3,
  2,  4,  3,  1,
  1,  2,  3, NA,
  2,  1,  3,  4,
  1,  2,  4,  3
), 6, byrow = TRUE)

colnames(R) <- c("Ada", "Ben", "Carol", "Daniel")
rownames(R) <- paste0("Race_", 1:6)

R

# Estimation using PlackettLuce package
rnk <- as.rankings(R)
fit_pl <- PlackettLuce(rnk, npseudo = 0, normal = list(mu = rep(0, 4), Sigma = diag(4)))


# Estimation using ROL model in Stan
ordered_ids <- apply(R, 1, order, na.last = NA)
stan_data <- list(
  ranked_ids = unname(unlist(ordered_ids)),
  num_competitors = unname(vapply(ordered_ids, length, 1)),
  M = 4,
  K = 6,
  N = sum(!is.na(R))
)
fit_rol_opt <- mod_rol$optimize(data = stan_data)
theta_hat <- fit_rol_opt$summary("theta")$estimate

# Comparing log-posterior
fit_pl$logposterior
fit_rol_opt$lp()

# Comparing estimates on the win probability scale
prob_win_pl <- exp(coef(fit_pl)) / sum(exp(coef(fit_pl)))
prob_win_rol <- exp(theta_hat) / sum(exp(theta_hat))
cbind(pl = prob_win_pl, rol = prob_win_rol, delta = prob_win_pl - prob_win_rol)

# Model works!
