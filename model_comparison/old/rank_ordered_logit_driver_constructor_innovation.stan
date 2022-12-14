functions {
  // likelihood; see http://www.glicko.net/research/multicompetitor.pdf
  real rank_ordered_logit(vector ordered_skills, int Mk) {
    real ll = 0;
    for (m in 1:(Mk - 1)) {
      ll += ordered_skills[m] - log_sum_exp(ordered_skills[m:Mk]);
    }
    return ll;
  }
  row_vector colSums(matrix input) {
    return ones_row_vector(rows(input)) * input;
  }
}
data {
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
  real<lower=0> sigma_driver;
  real<lower=0> sigma_constructor;

  // innovation parameters
  real<lower=0> tau_driver; // innovation variance driver
  real<lower=0> tau_constructor; // innovation variance constructor

  // innovations must sum to 0, column-wise, so we need num_drivers - 1 degrees of freedom
  // see https://mc-stan.org/docs/2_21/stan-users-guide/parameterizing-centered-vectors.html
  matrix[num_drivers - 1, num_seasons - 1] innovation_driver_raw;
  matrix[num_constructors - 1, num_seasons - 1] innovation_constructor_raw;
}
transformed parameters {
  // these are the actual innovations
  matrix[num_drivers, num_seasons - 1] innovation_driver;
  matrix[num_constructors, num_seasons - 1] innovation_constructor;

  innovation_driver = append_row(innovation_driver_raw, -colSums(innovation_driver_raw));
  innovation_constructor = append_row(innovation_constructor_raw, -colSums(innovation_constructor_raw));

  // log-worths, or skill parameters; one for each driver & constructor
  matrix[num_drivers, num_seasons] theta_driver;
  matrix[num_constructors, num_seasons] theta_constructor;

}
model {
  // Prior for first race
  sigma_driver ~ cauchy(0, 2.5);
  sigma_constructor ~ cauchy(0, 2.5);
  col(theta_driver, 1) ~ normal(0, sigma_driver * inv(sqrt(1 - inv(num_drivers))));
  col(theta_constructor, 1) ~ normal(0, sigma_constructor * inv(sqrt(1 - inv(num_constructors))));

  // prior on innovation
  tau_driver^2 ~ inv_gamma(3.0, 0.1);
  tau_constructor^2 ~ inv_gamma(3.0, 0.1);
  to_vector(innovation_driver) ~ normal(0, tau_driver * inv(sqrt(1 - inv(num_drivers))));
  to_vector(innovation_constructor) ~ normal(0, tau_constructor * inv(sqrt(1 - inv(num_constructors))));


  // Likelihood
  int pos = 1;
  for (k in 1:num_races) {
    int Mk = num_entrants[k];
    int s = season_id[k];

    vector[num_drivers] theta_driver_k;
    vector[num_drivers] theta_constructor_k;

    if (s == 1) {
      theta_driver_k = col(theta_driver, 1);
      theta_constructor_k = col(theta_constructor, 1);
    } else {
      theta_driver_k = col(theta_driver, s - 1) + col(innovation_driver, s - 1);
      theta_constructor_k = col(theta_constructor, s - 1) + col(innovation_constructor, s - 1);
    }

    vector[Mk] driver_skills = theta_driver_k[segment(ranked_driver_ids, pos, Mk)];
    vector[Mk] constructor_skills = theta_constructor_k[segment(ranked_constructor_ids, pos, Mk)];

    target += rank_ordered_logit(driver_skills + constructor_skills, Mk);
    pos = pos + Mk;
  }
}

