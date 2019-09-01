
calc_avg_mort <- function(df_params, hazard = "Gompertz"){
  with(as.list(df_params),{
  lambda_b_hat <- switch (hazard,
    "Gompertz" = mu0*exp(alpha*a0)*(exp(alpha*trial_time) - 1)/(alpha * trial_time)
  )
  return(lambda_b_hat)
  }
  )
}

calc_dshr_from_ohr <- function(df_params, hazard = "Gompertz"){
  with(as.list(df_params),{
    ## Average background mortality for the trial over trial_time years for a 
    ## cohort with initial age a0,
    lambda_b_hat <- calc_avg_mort

    dshr <- (ohr*(lambda_b_hat + mu_Dis) - lambda_b_hat)/mu_Dis
    return(dshr)
  }
  )
}
