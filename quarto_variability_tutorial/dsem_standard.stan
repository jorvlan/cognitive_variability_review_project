
//-----------------------------------------------//
// Dynamic Structural Equation Model (DSEM):
// Estimates intra-individual trajectories in Y (alpha) over time,
// while simultaneously taking into account the autocorrelation (phi), systematic change by a linear trend (beta),
// and residual variance (sigma), and allowing individuals to differ in these effects (random effects).
// The model code is written in Stan and vectorized 
// see https://mc-stan.org/ and https://mc-stan.org/docs/2_18/stan-users-guide/vectorization.html
// To make the model specification easier to digest, we provide short comments for each section.
//------------------------------------------------//

data {
  int<lower = 1> N; // total number of trials
  int<lower = 1> N_subj; // total number of subjects
  array[N] int<lower=1, upper=N_subj> subj; // subject index for each trial
  vector[N] Y; // outcome variable
  vector[N] Y_tm1; // lagged outcome variable (Y at t-1)
  vector[N] time; // rescaled trial number of Y
  vector[N] time_tm1; // rescaled trial number of the previous trial
  vector[N_subj] mtime; // within-person mean time (for centering the linear trend)
}
parameters {
  real sigma; // Population-level residual SD (log-scale)
  vector<lower=0>[4] tau_u; // Random effects SDs (4 total, 1 for each parameter)
  real alpha; // Population-level mean
  real beta; // Population-level trend
  real phi; // Population-level autocorrelation
  matrix[4, N_subj] z_u; // Random effects (non-centered parameterization)
  cholesky_factor_corr[4] L_u; // Cholesky factor of random effects correlation matrix
                               // see https://bruno.nicenboim.me/bayescogsci/ch-complexstan.html#sec-corrstan
}
transformed parameters {
  matrix[N_subj,4] u; // subject-specific random effects (alpha, beta, phi, sigma)
  // reconstruct random effects from Cholesky factor and standardized deviations
  u = transpose(diag_pre_multiply(tau_u, L_u) * z_u);
}

model {
  // subject-specific trajectory at the current and at the previous trial
  vector[N] mu      = alpha + u[subj,1] + (time     - mtime[subj]) .* (beta + u[subj,2]);
  vector[N] mu_prev = alpha + u[subj,1] + (time_tm1 - mtime[subj]) .* (beta + u[subj,2]);

  // population-level priors
  alpha ~ normal(0, 50); // population-level mean prior
  phi   ~ normal(0, 50); // population-level autocorrelation prior
  beta  ~ normal(0, 50); // population-level trend prior
  sigma ~ normal(0, 50); // population-level log residual SD prior
  tau_u ~ cauchy(0, 2); // random-effects SDs prior

  // priors for non-centered parameterization
  to_vector(z_u) ~ std_normal();
  L_u ~ lkj_corr_cholesky(1); // LKJ prior 1 (uniform across correlation matrices)

  // likelihood
  // mean = person-specific trajectory +
  //        autocorrelation of the previous deviation from that trajectory
  // residual SD = exp(sigma + u4); on log-scale to assume normal distribution which simplifies estimation
  Y ~ normal(mu + (Y_tm1 - mu_prev) .* (phi + u[subj,3]),
             exp(sigma + u[subj,4]));
}
generated quantities {
  // retrieve correlation matrix from Cholesky factor
  corr_matrix[4] rho_u = L_u * L_u';
}

