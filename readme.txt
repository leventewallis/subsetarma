The effectiveness of variable selection based on the
best subset principle on time-series data – how often
the theoretical optimum is identified
This folder contains the R scripts for my BSc thesis.
ARMA_epit.R 
  ARMA_epit function - returns the coefficients, BIC, and different criterias for the specified subset arma model
  inputs:
    egyed - latent variables - which variables (lags) are selected (only 1s and 0s)
    Y - time series
    alpha - significance level
  neg_loglikelihood function - returns the - loglikelihood, important for calculating the coefficients
  adatokkiszam function immportant for calculating the residuals
