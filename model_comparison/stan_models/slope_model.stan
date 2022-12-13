// hierarchical rank-ordered logit regression in stan
functions {
  // likelihood; see http://www.glicko.net/research/multicompetitor.pdf
  real rank_ordered_logit(vector ordered_skills, int Mk) {
    real ll = 0;
    for (m in 1:(Mk - 1)) {
      ll += ordered_skills[m] - log_sum_exp(ordered_skills[m:Mk]);
    }
    return ll;
  }
}
data {
  int<lower=0> num_obs;
  int<lower=0> num_drivers;
  int<lower=0> num_teams;
  int<lower=0> num_races;
  int<lower=0> num_seasons;

  array[num_obs] int<lower=1, upper=num_drivers> ranked_driver_ids; // driver orderings for each race
  array[num_obs] int<lower=1, upper=num_teams> ranked_team_ids; // constructor orderings for each race

  array[num_races] int<lower=1> num_entrants; // number of entrants for each race
  array[num_races] int<lower=1> season_id; // which season is this race in?
}

parameters {
  vector[num_drivers] theta_driver_intercept_raw;
  vector[num_teams] theta_team_intercept_raw;
  vector[num_drivers] theta_driver_slope_raw;
  vector[num_teams] theta_team_slope_raw;

  real<lower=0> tau_driver_intercept;
  real<lower=0> tau_team_intercept;
  real<lower=0> tau_driver_slope;
  real<lower=0> tau_team_slope;
}

transformed parameters {
  // non-centered parametrization
  vector[num_drivers] theta_driver_intercept;
  vector[num_teams] theta_team_intercept;
  vector[num_drivers] theta_driver_slope;
  vector[num_teams] theta_team_slope;

  theta_driver_intercept = tau_driver_intercept*theta_driver_intercept_raw;
  theta_team_intercept = tau_team_intercept*theta_team_intercept_raw;
  theta_driver_slope = tau_driver_slope*theta_driver_slope_raw;
  theta_team_slope = tau_team_slope*theta_team_slope_raw;
}

model {
  // priors
  tau_driver_intercept ~ student_t(3, 0, 2.5);
  tau_team_intercept ~ student_t(3, 0, 2.5);
  tau_driver_slope ~ student_t(3, 0, 2.5);
  tau_team_slope ~ student_t(3, 0, 2.5);

  theta_driver_intercept_raw ~ std_normal();
  theta_team_intercept_raw ~ std_normal();
  theta_driver_slope_raw ~ std_normal();
  theta_team_slope_raw ~ std_normal();

  // ROL likelihood
  int pos = 1; // current position in outcome vectors
  for (k in 1:num_races) {
    int m_k = num_entrants[k];
    int s = season_id[k];

    array[m_k] int driver_idx = segment(ranked_driver_ids, pos, m_k);
    array[m_k] int team_idx = segment(ranked_team_ids, pos, m_k);

    vector[m_k] driver_skills = theta_driver_intercept[driver_idx] + (s-5)*theta_driver_slope[driver_idx];
    vector[m_k] team_skills = theta_team_intercept[team_idx] + (s-5)*theta_team_slope[team_idx];

    target += rank_ordered_logit(driver_skills + team_skills, m_k);
    pos = pos + m_k;
  }

}

