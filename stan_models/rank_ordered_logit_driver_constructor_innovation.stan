functions {
  real rank_ordered_logit(vector ordered_skills, int Mk) {
    real ll = 0;
    for (m in 1:(Mk - 1)) {
      ll += ordered_skills[m] - log_sum_exp(ordered_skills[m:Mk]);
    }
    return ll;
  }
}
data {
  int<lower=1> M_d; // total number of drivers
  int<lower=1> M_c; // total number of constructors
  int<lower=1> K; // total number of rounds
  int<lower=1> S; // total number of seasons
  int<lower=1> N; // total number of observations (ranks)


  // ragged array type race result encoding
  // ranked_ids is the rank-ordered driver/constructor ids for each race, concatenated
  array[N] int<lower=1, upper=M_d> ranked_driver_ids;
  array[N] int<lower=1, upper=M_c> ranked_constructor_ids;
  array[K] int<lower=1> num_entrants; // number of entrants for each race
  array[K] int<lower=1, upper=S> season_id; // which season is the race in?
}
transformed data {
  matrix[M_d, M_d] ones_d =
}

parameters {
  // log-worths, or skill parameters; one for each driver & constructor
  matrix[M_d, S] theta_driver;
  matrix[M_c, S] theta_constructor;

  // variance for first race skill parameters
  real<lower=0> sigma_driver;
  real<lower=0> sigma_constructor;

  // innovation parameters
  real<lower=0> tau_driver; // innovation
  real<lower=0> tau_constructor; // innovation
}
model {
  // Prior for first race
  sigma_driver ~ cauchy(0, 2.5);
  sigma_constructor ~ cauchy(0, 2.5);
  col(theta_driver, 1) ~ normal(0, sigma_driver);
  col(theta_constructor, 1) ~ normal(0, sigma_constructor);

  // prior on innovation
  tau_driver^2 ~ inv_gamma(3.0, 1.5);
  tau_constructor^2 ~ inv_gamma(3.0, 1.5);

  // Likelihood
  int s = 1; // current season
  int pos = 1;
  for (k in 1:K) {
    int Mk = num_entrants[k];

    if (k > 1 && s < season_id[k]) {
      s = season_id[k];
      cov_matrix innovation_d = tau_driver^2*(identity_matrix(M_d) - ones
      // innovation prior
      col(theta_driver, s) ~ normal(col(theta_driver, s-1), tau_driver);
      col(theta_constructor, s) ~ normal(col(theta_constructor, s-1), tau_driver);
    }

    vector[Mk] driver_skills = col(theta_driver, s)[segment(ranked_driver_ids, pos, Mk)];
    vector[Mk] constructor_skills = col(theta_constructor, s)[segment(ranked_constructor_ids, pos, Mk)];

    target += rank_ordered_logit(driver_skills + constructor_skills, Mk);
    pos = pos + Mk;
  }
}

