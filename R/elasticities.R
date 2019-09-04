#' Elasticities of disease-specific hazard ratio (dsHR)
#' 
#' \code{comp_elast_gompz} computes the elasticities of dsHR with respect to 
#' parameters obtained from a clinical trial
#' @param ohr Overall hazard ratio
#' @param mu_Dis Disease-specific mortality rate
#' @param n_age_init Initial age of the cohort in the trial
#' @param trial_time Length of the trial
#' @param mu0 Age-independent mortality rate coefficient
#' @param alpha Age-dependent mortality rate coefficient
#' @param hazard The hazard function to be used for background mortality. 
#' Either empirical (e.g., from life tables) or a continuous-time functional 
#' form.
#' @param mortality A \code{data.frame} with two columns. The first column 
#' should have age in yearly increments and the second column should have 
#' the mortality rate corresponding to the age. If \code{NULL}, the function
#' will use \code{\link{all_cause_mortality}}.
#' @return 
#' Elasticities of dsHR with respect to trial parameters.
#' @export
comp_elast_gompz <- function(ohr = 0.55, mu_Dis = 0.05, 
                             n_age_init = 50, trial_time = 5,
                             mu0 = NULL, alpha = NULL, 
                             hazard = c("exponential"),
                             mortality = NULL){
  ### Sanity checks
  ## Check that `hazard` contains valid value
  hazard <- match.arg(hazard)
  ## Check that mu0 and alpha are supplied when not empirical
  if(hazard %in% c("exponential")){
    if (is.null(mu0) | is.null(alpha)){
      stop("`mu0` and `alpha` should not be NULL when hazard is either exponential, linear, or geometric")
    }
  }
  
  ### Function implementation
  lambda_b_hat <- calc_avg_mort(n_age_init = n_age_init, 
                                trial_time = trial_time,
                                mu0 = mu0, 
                                alpha = alpha,
                                hazard = hazard, 
                                mortality = mortality)
  
  e_dshr_ohr <- ((lambda_b_hat + mu_Dis) * ohr * mu_Dis)/(ohr * (lambda_b_hat + mu_Dis) - lambda_b_hat)
  l_e_dshr_out <- list(e_dshr_ohr = e_dshr_ohr)
  return(l_e_dshr_out)
}