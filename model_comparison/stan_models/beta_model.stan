// hierarchical beta regression in stan
// inspired by https://github.com/daltonhance/stan_beta_reg

data {
  int<lower=0> num_obs;
  int<lower=0> num_drivers;
  int<lower=0> num_teams;
  int<lower=0> num_seasons;

  vector<lower=0, upper=1>[num_obs] y; // outcome

  array[num_obs] int<lower=1> driver_id;
  array[num_obs] int<lower=1> team_id;
  array[num_obs] int<lower=1> season_id;
}

parameters {
  vector[num_drivers] theta_driver_raw;
  vector[num_teams] theta_team_raw;
  matrix[num_drivers, num_seasons] theta_driver_season_raw;
  matrix[num_teams, num_seasons] theta_team_season_raw;

  real<lower=0> tau_driver;
  real<lower=0> tau_team;
  real<lower=0> tau_driver_season;
  real<lower=0> tau_team_season;

  real<lower=0> phi;
}

transformed parameters {
  // non-centered parametrization
  vector[num_drivers] theta_driver;
  vector[num_teams] theta_team;
  matrix[num_drivers, num_seasons] theta_driver_season;
  matrix[num_teams, num_seasons] theta_team_season;

  theta_driver = tau_driver * theta_driver_raw;
  theta_team = tau_team * theta_team_raw;
  theta_driver_season = tau_driver_season * theta_driver_season_raw;
  theta_team_season = tau_team_season * theta_team_season_raw;

  vector<lower=0,upper=1>[num_obs] mu; // transformed linear predictor for mean of beta distribution
  vector<lower=0>[num_obs] alpha;      // parameter for beta distn
  vector<lower=0>[num_obs] beta;       // parameter for beta distn

  for (n in 1:num_obs) {
    real theta_driver_n = theta_driver[driver_id[n]] + theta_driver_season[driver_id[n], season_id[n]];
    real theta_team_n = theta_team[team_id[n]] + theta_team_season[team_id[n], season_id[n]];
    mu[n] = inv_logit(theta_driver_n + theta_team_n);
  }

  alpha = mu * phi;
  beta = (1.0 - mu) * phi;
}

model {
  // priors
  tau_driver ~ student_t(3, 0, 2.5);
  tau_team ~ student_t(3, 0, 2.5);
  tau_driver_season ~ student_t(3, 0, 2.5);
  tau_team_season ~ student_t(3, 0, 2.5);

  theta_driver_raw ~ std_normal();
  theta_team_raw ~ std_normal();
  to_vector(theta_driver_season_raw) ~ std_normal();
  to_vector(theta_team_season_raw) ~ std_normal();

  phi ~ student_t(3, 0, 2.5);

  // beta regression
  y ~ beta(alpha, beta);

}

