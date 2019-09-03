context("testing dshr_from_ohr.R")

library(dplyr)    # For data manipulation
library(dshr)

#### Unit tests start ####
test_that("correct outputs exponential hazard", {
  ### Define inputs
  n_age_init <- 50   # Initial age of cohort in trial
  trial_time <- 1    # Length of clinial trial in years
  ohr        <- 0.55 # Overall hazard ratio (oHR)
  mu_Dis     <- 0.05 # Disease-specific mortality rate  
  
  ### Estimate exponential hazard functional form directly
  n_age_max  <- n_age_init + trial_time
  # n_age_max  <- 100
  v_ages <- n_age_init:n_age_max
  
  fit_exp_50 <- lm(log(Total) ~ Age, 
                     data = all_cause_mortality[(v_ages + 1), ])
  coef_exp_50 <- coef(fit_exp_50)
  mu0 <- exp(coef_exp_50[1])
  alpha <- coef_exp_50[2]
  v_params_exp <- c(mu0 = mu0, alpha = alpha)
  
  ### Estimate exponential hazard functional form with function 
  v_coef_hazard <- est_hazard_params(n_age_init = n_age_init, 
                                     trial_time = trial_time, 
                                     hazard = "exponential")
  
  expect_equal(as.numeric(v_coef_hazard), as.numeric(v_params_exp))
})
