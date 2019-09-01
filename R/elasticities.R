calc_elast_gompz <- function(mu0, alpha, ohr, mu_Dis, a0, trial_time){
  lambda_b_hat <- mu0*exp(alpha*a0)*(exp(alpha*trial_time) - 1)/(alpha * trial_time)
  e_ohr <- ((lambda_b_hat + mu_Dis) * ohr * mu_Dis)/(ohr * (lambda_b_hat + mu_Dis) - lambda_b_hat)
  v_e_out <- c(e_ohr = e_ohr)
  return(v_e_out)
}