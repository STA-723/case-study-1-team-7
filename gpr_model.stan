// Model changed (so estimation is actually feasible in time)
// need to be changed
data {
  // All data are assumed properly centered & scaled
  int<lower=0> N; // Total number of observations
  int<lower=0> J; // Number of groups
  int<lower=0> P; // Number of predictors
  int<lower=1, upper=J> group[N]; // Group coding for each observation
  vector[N] y; // Response
  matrix[N, P] x; // Predictors
}
parameters {
  real alpha_group[J]; // Within-center intercepts
  real alpha; // Between-center intercept
  vector[P] beta_group[J]; // Within-center coefficients
  vector[P] beta; // Between-center coefficients
  real<lower=0> sigma; // Error std. dev.
  real<lower=0> tau; // Group(center)-level std.dev.
}
model {
  // // Brackets define a local scope for loop speed optimization
  {
    // block for data likelihood
    vector[N] groupMean; // regression linear predictor for each group
    for (n in 1:N)
      groupMean[n] = alpha_group[group[n]] + x[n] * beta_group[group[n]];
    y ~ normal(groupMean, sigma);
  }
  // Prior block for beta's (group-level)
  for (j in 1:J) {
    alpha_group[j] ~ normal(alpha, tau);
    beta_group[j] ~ normal(beta, tau);
  }
  // Hyper-priors: coefficients and variance parameters
  alpha ~ normal(0, 1);
  beta ~ normal(to_vector(rep_array(0, P)), 1); // Scale estimated from data & iid
  sigma ~ normal(0, 1); // Half-cauchy, restricted to positive real line
  tau ~ normal(0, 1); // Similar as above
}
