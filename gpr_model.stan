data {
  int<lower=0> N; // Total number of observations
  int<lower=0> J; // Number of groups
  int<lower=0> P; // Number of predictors
  int<lower=0> K; // Number of PCB byproducts (additional latent layer)
  int<lower=1, upper=J> group[N]; // Group coding for each observation
  vector[N] y; // Response
  matrix[N, P-K] x; // Predictors (Excl. PCB's)
  matrix[N, K] pcb; // PCB as a separate predictor vector
}
parameters {
  vector[P-K] beta_group[J]; // Within-center coefficients
  vector[K] beta_pcb[J]; // Coefficients for PCB's (within-center)
  vector[P-K] beta; // Between-center coefficients
  real beta_pcb_within[J]; // "Total" PCB effect (within-center)
  real beta_pcb_between; // "Total" PCB effect (between-center)
  real<lower=0> sigma; // Error std. dev.
  real<lower=0> tau; // Group(center)-level std.dev.
  real<lower=0> sigma_pcb; // Within-std. dev. for different PCB byproducts
}
model {
  // // Brackets define a local scope for loop speed optimization
  {
    // block for data likelihood
    vector[N] groupMean; // regression linear predictor for each group
    for (n in 1:N)
      groupMean[n] = pcb[n] * beta_pcb[group[n]] + x[n] * beta_group[group[n]];
    y ~ normal(groupMean, sigma);
  }
  // Each within-center, different PCB coefficients are shrunk to latent, "total"
  // PCB effect
  for (j in 1:J) {
    // FIX LIKELY TO CAUSE MISMATCH
    beta_pcb[j] ~ normal(beta_pcb_within[j], sigma_pcb);
  }
  // Prior block for beta's (group-level)
  for (j in 1:J) {
    beta_group[j] ~ normal(beta, tau);
    beta_pcb_within[j] ~ normal(beta_pcb_between, tau);
  }
  // Hyper-priors: coefficients and variance parameters
  for (p in 1:(P-K)) {
    beta[p] ~ normal(0, 1);
  }
  sigma ~ cauchy(0, 1);
  sigma_pcb ~ normal(0, 1);
  tau ~ normal(0, 1);
}
