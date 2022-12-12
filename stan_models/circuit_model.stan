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

  array[num_races] int<lower=0, upper=1> prm_circuit; // is this track a permanent circuit?
}

parameters {
  vector[num_drivers] theta_driver;
  vector[num_teams] theta_team;
  matrix[num_drivers, num_seasons] theta_driver_season;
  matrix[num_teams, num_seasons] theta_team_season;

  vector[num_drivers] circuit_effect;

  real<lower=0> tau_driver;
  real<lower=0> tau_team;
  real<lower=0> tau_driver_season;
  real<lower=0> tau_team_season;
  real<lower=0> tau_circuit;
}

transformed parameters {

}

model {
  // priors
  tau_driver ~ student_t(3, 0, 2.5);
  tau_team ~ student_t(3, 0, 2.5);
  tau_driver_season ~ student_t(3, 0, 2.5);
  tau_team_season ~ student_t(3, 0, 2.5);
  tau_circuit ~ student_t(3, 0, 2.5);

  theta_driver ~ normal(0, tau_driver);
  theta_team ~ normal(0, tau_team);
  to_vector(theta_driver_season) ~ normal(0, tau_driver_season);
  to_vector(theta_team_season) ~ normal(0, tau_team_season);
  circuit_effect ~ normal(0, tau_circuit);

  // ROL likelihood
  int pos = 1; // current position in outcome vectors
  for (k in 1:num_races) {
    int m_k = num_entrants[k];
    int s = season_id[k];

    array[m_k] int driver_idx = segment(ranked_driver_ids, pos, m_k);
    array[m_k] int team_idx = segment(ranked_team_ids, pos, m_k);

    vector[m_k] driver_skills =
      theta_driver[driver_idx] +
      col(theta_driver_season, s)[driver_idx] +
      prm_circuit[k]*circuit_effect[driver_idx];

    vector[m_k] team_skills = theta_team[team_idx] + col(theta_team_season, s)[team_idx];

    target += rank_ordered_logit(driver_skills + team_skills, m_k);
    pos = pos + m_k;
  }

}

generated quantities {
  // compute log_likelihood again!
  vector[num_races] log_lik;
  int pos = 1;
  for (k in 1:num_races) {
    int m_k = num_entrants[k];
    int s = season_id[k];

    array[m_k] int driver_idx = segment(ranked_driver_ids, pos, m_k);
    array[m_k] int team_idx = segment(ranked_team_ids, pos, m_k);

    vector[m_k] driver_skills =
      theta_driver[driver_idx] +
      col(theta_driver_season, s)[driver_idx] +
      prm_circuit[k]*circuit_effect[driver_idx];

    vector[m_k] team_skills = theta_team[team_idx] + col(theta_team_season, s)[team_idx];

    log_lik[k] = rank_ordered_logit(driver_skills + team_skills, m_k);
    pos = pos + m_k;
  }

}

