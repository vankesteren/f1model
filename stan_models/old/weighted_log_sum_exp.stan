functions {
  real weighted_log_sum_exp(vector x, vector w) {
    int len = num_elements(x);

    real a = x[1];
    real r = w[1];

    if (len == 1) return a + log(r);

    for (i in 2:len) {
      if (x[i] <= a) {
        // standard computation
        r += w[i] * exp(x[i] - a);
      } else {
        r *= exp(a - x[i]);
        r += w[i];
        a = x[i];
      }
    }

    return a + log(r);
  }
}
