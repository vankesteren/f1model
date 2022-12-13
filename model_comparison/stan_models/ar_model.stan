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
  vector[num_drivers] theta_driver_raw;
  vector[num_teams] theta_team_raw;
  matrix[num_drivers, num_seasons] theta_driver_season_raw;
  matrix[num_teams, num_seasons] theta_team_season_raw;

  // autoregressive parameters
  vector<lower=-1, upper=1>[num_drivers] ar_driver;
  vector<lower=-1, upper=1>[num_teams] ar_team;

  real<lower=0> tau_driver;
  real<lower=0> tau_team;
  real<lower=0> tau_driver_season;
  real<lower=0> tau_team_season;
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

  matrix[num_drivers, num_seasons] driver_skill;
  matrix[num_teams, num_seasons] team_contribution;

  // cluster mean centered autoregression
  // see, e.g., https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01492/full
  driver_skill[,1] = theta_driver + theta_driver_season[,1];
  team_contribution[,1] = theta_team + theta_team_season[,1];
  for (s in 2:num_seasons) {
    driver_skill[,s] = theta_driver +
      ar_driver .* (driver_skill[,s - 1] - theta_driver) +
      theta_driver_season[,s];
    team_contribution[,s] = theta_team +
      ar_team .* (team_contribution[,s - 1] - theta_team) +
      theta_team_season[,s];
  }
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

  ar_driver ~ normal(0, 2);
  ar_team ~ normal(0, 2);


  // ROL likelihood
  int pos = 1; // current position in outcome vectors
  for (k in 1:num_races) {
    int m_k = num_entrants[k];
    int s = season_id[k];
    vector[m_k] driver_skills = col(driver_skill, s)[segment(ranked_driver_ids, pos, m_k)];
    vector[m_k] team_skills = col(team_contribution, s)[segment(ranked_team_ids, pos, m_k)];
    target += rank_ordered_logit(driver_skills + team_skills, m_k);
    pos = pos + m_k;
  }

}

