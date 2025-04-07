data {
  int<lower=1> N;       // Number of observations
  int<lower=1> K;       // Number of basis functions (columns in X)
  matrix[N, K] X;       // B-spline design matrix
  vector[N] y;          // Response variable
}

parameters {
  vector[K] beta;       // Coefficients for the spline basis
  real alpha;           // Intercept
  real<lower=0> sigma;  // Standard deviation of residuals
}

model {
  y ~ normal(alpha + X * beta, sigma);  // Linear combination of spline basis
}
