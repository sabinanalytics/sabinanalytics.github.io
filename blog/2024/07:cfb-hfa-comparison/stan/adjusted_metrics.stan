// adjusted_metrics.stan
//this model takes game variables and tries to estimate team effects
data {
  int<lower=1> N;  // Number of observations
  int<lower=1> p; // number of fixed effects
  vector[N] response_variable;  // Response variable
  matrix[N, p] fixed_effects;
  int<lower=1> n_team;  // Number of unique levels for team
  int<lower=1> n_opp;   // Number of unique levels for opp
  int<lower=1, upper=n_team> team_ind[N];  // Season team grouping variable
  int<lower=1, upper=n_opp> opp_ind[N];    // Season opp grouping variable
  // vector[N] plays_weight;  // Weights for observations
  
  //prior mean and std deviations for each team's offense and defense
  vector[n_team] team_prior_mean;
  vector[n_opp] opp_prior_mean;
  vector[n_team] team_prior_sd;
  vector[n_opp] opp_prior_sd;
  
}

parameters {
  //real global_intercept;
  vector[p] coefficients;
  vector[n_team] team_effects_raw;
  vector[n_opp] opp_effects_raw;
  real<lower=0> sigma;  // Residual standard deviation
}

transformed parameters {
  vector[N] linear_predictor;

  for (i in 1:N) {
    linear_predictor[i] = //global_intercept +
      fixed_effects[i]*coefficients +
      team_effects_raw[team_ind[i]] - //negative because good defense should be a positive value
      opp_effects_raw[opp_ind[i]];
  }
}

model {
  // Priors for varying effects
  team_effects_raw ~ normal(team_prior_mean, team_prior_sd);  // Adjust the scale as needed
  opp_effects_raw ~ normal(opp_prior_mean, opp_prior_sd);   // Adjust the scale as needed

  // Likelihood
  response_variable ~ normal(linear_predictor, sigma);
}

// generated quantities {
//   vector[N] log_lik;
// 
//   // Calculate log-likelihood for posterior predictive checks
//   for (i in 1:N) {
//     log_lik[i] = normal_lpdf(response_variable[i] | linear_predictor[i], sigma);
//   }
// }

