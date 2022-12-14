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
  real<lower=0> inno_relax;

  int<lower=1> num_drivers;      // total number of drivers
  int<lower=1> num_constructors; // total number of constructors
  int<lower=1> num_races;        // total number of rounds
  int<lower=1> num_seasons;      // total number of seasons
  int<lower=1> num_observations; // total number of observations (ranks)

  // Ragged array data structure
  array[num_observations] int<lower=1, upper=num_drivers> ranked_driver_ids; // driver orderings for each race
  array[num_observations] int<lower=1, upper=num_constructors> ranked_constructor_ids; // constructor orderings for each race
  array[num_races] int<lower=1> num_entrants; // number of entrants for each race
  array[num_races] int<lower=1, upper=num_seasons> season_id; // which season is the race in?
}
parameters {
  // variance for first race skill parameters
  real<lower=0> sigma_start_driver;
  real<lower=0> sigma_start_constructor;

  // first race estimate
  vector[num_drivers] start_driver;
  vector[num_constructors] start_constructor;

  // innovation parameters
  real<lower=0> tau_driver; // innovation variance driver
  real<lower=0> tau_constructor; // innovation variance constructor

  // these are the innovations
  matrix[num_drivers, num_seasons - 1] innovation_driver;
  matrix[num_constructors, num_seasons - 1] innovation_constructor;
}
transformed parameters {
  // log-worths, or skill parameters; one for each driver & constructor
  matrix[num_drivers, num_seasons] theta_driver;
  matrix[num_constructors, num_seasons] theta_constructor;

  theta_driver[, 1] = sigma_start_driver * start_driver;
  theta_constructor[, 1] = sigma_start_constructor * start_constructor;

  for (s in 2:num_seasons) {
    theta_driver[, s] = col(theta_driver, s - 1) + col(innovation_driver, s - 1);
    theta_constructor[, s] = col(theta_constructor, s - 1) + col(innovation_constructor, s - 1);
  }

}
model {
  // Prior for first race
  sigma_start_driver ~ cauchy(0, 2.5);
  sigma_start_constructor ~ cauchy(0, 2.5);

  // prior on innovation
  tau_driver^2 ~ inv_gamma(3.0, inno_relax);
  tau_constructor^2 ~ inv_gamma(3.0, inno_relax);
  to_vector(innovation_driver) ~ normal(0, tau_driver);
  to_vector(innovation_constructor) ~ normal(0, tau_constructor);


  // Likelihood
  int pos = 1;
  for (k in 1:num_races) {
    int Mk = num_entrants[k];
    int s = season_id[k];

    vector[Mk] driver_skills = col(theta_driver, s)[segment(ranked_driver_ids, pos, Mk)];
    vector[Mk] constructor_skills = col(theta_constructor, s)[segment(ranked_constructor_ids, pos, Mk)];

    target += rank_ordered_logit(driver_skills + constructor_skills, Mk);
    pos = pos + Mk;
  }
}

