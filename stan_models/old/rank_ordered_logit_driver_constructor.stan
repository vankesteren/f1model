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
  int<lower=1> N; // total number of observations (ranks)

  // ragged array type race result encoding
  // ranked_ids is the rank-ordered driver/constructor ids for each race, concatenated
  array[N] int<lower=1, upper=M_d> ranked_driver_ids;
  array[N] int<lower=1, upper=M_c> ranked_constructor_ids;
  array[K] int<lower=1> num_entrants; // group sizes
}
parameters {
  // log-worths, or skill parameters; one for each driver
  real<lower=0> sigma_driver;
  real<lower=0> sigma_constructor;
  vector[M_d] theta_driver;
  vector[M_c] theta_constructor;
}
model {
  // Prior
  sigma_driver ~ cauchy(0, 2.5);
  sigma_constructor ~ cauchy(0, 2.5);
  theta_driver ~ normal(0, sigma_driver);
  theta_constructor ~ normal(0, sigma_constructor);

  // Likelihood
  int pos;
  pos = 1;
  for (k in 1:K) {
    int Mk = num_entrants[k];
    vector[Mk] driver_skills = theta_driver[segment(ranked_driver_ids, pos, Mk)];
    vector[Mk] constructor_skills = theta_constructor[segment(ranked_constructor_ids, pos, Mk)];
    target += rank_ordered_logit(driver_skills + constructor_skills, Mk);
    pos = pos + Mk;
  }
}

