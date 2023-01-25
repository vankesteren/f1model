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
  int<lower=1> M; // total number of competitors
  int<lower=1> K; // total number of rounds
  int<lower=1> N; // total number of observations (ranks)

  // ragged array type race result encoding
  // rank_ids is the rank-ordered driver ids for each race, concatenated
  array[N] int<lower=1, upper=M> ranked_ids;
  array[K] int<lower=1> num_competitors; // group sizes
}
parameters {
  // log-worths, or skill parameters; one for each driver
  vector[M] theta;
}
model {
  // Prior
  theta ~ normal(0, 1);

  // Likelihood
  int pos;
  pos = 1;
  for (k in 1:K) {
    int Mk = num_competitors[k];
    vector[Mk] ordered_skills = theta[segment(ranked_ids, pos, Mk)];
    target += rank_ordered_logit(ordered_skills, Mk);
    pos = pos + num_competitors[k];
  }
}

