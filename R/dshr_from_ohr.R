#' Disease-specific hazard ratio (dsHR) from an overall hazard ratio (oHR)
#' 
#' \code{calc_dshr_from_ohr} calculates a dsHR from an oHR for a cohort with
#' initial age \code{n_age_init} in a trial with duration of \code{trial_time} years 
#' with a disease-specific mortality rate, \code{mu_Dis}, under a specific 
#' continuous-time hazard function for background mortality.
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
#' @section Details:
#' This function requires the average background mortality of the cohort, which
#' is computed with the function \code{\link{calc_avg_mort}}.
#' @return 
#' The disease-specific hazard ratio (dsHR)
#' @export
#' 
#' @examples 
#' n_age_init <- 50   # Initial age of cohort in trial
#' trial_time <- 5    # Length of clinial trial in years
#' ohr        <- 0.55 # Overall hazard ratio (oHR)
#' mu_Dis     <- 0.05 # Disease-specific mortality rate
#' \dontrun{
#'    dshr_emp <- calc_dshr_from_ohr(ohr = ohr, mu_Dis = mu_Dis,
#'    n_age_init = n_age_init, 
#'    trial_time = trial_time, 
#'    hazard = "empirical")
#'    dshr_emp
#' }
calc_dshr_from_ohr <- function(ohr = 0.55, mu_Dis = 0.05, 
                               n_age_init = 50, trial_time = 5,
                               mu0 = NULL, alpha = NULL,
                               hazard = c("empirical", "exponential", "linear", "geometric"), 
                               mortality = NULL){
  ### Sanity checks
  ## Check that `hazard` contains valid value
  hazard <- match.arg(hazard)
  ## Check that mu0 and alpha are supplied when not empirical
  if(hazard %in% c("exponential", "linear", "geometric")){
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
  ## Average background mortality for the trial over trial_time years for a 
  ## cohort with initial age n_age_init,
  dshr <- (ohr*(lambda_b_hat + mu_Dis) - lambda_b_hat)/mu_Dis
  return(dshr)
}

#' Overall hazard ratio (oHR) from a disease-specific hazard ratio (dsHR)
#' 
#' \code{calc_ohr_from_dshr} calculates an oHR from a dsHR for a cohort with
#' initial age \code{n_age_init} in a trial with duration of \code{trial_time} years 
#' with a disease-specific mortality rate, \code{mu_Dis}, under a specific 
#' continuous-time hazard function for background mortality.
#' @param dshr Disease-specific hazard ratio
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
#' @section Details:
#' This function requires the average background mortality of the cohort, which
#' is computed with the function \code{\link{calc_avg_mort}}.
#' @return 
#' The overall hazard ratio (oHR)
#' @export
#' 
#' @examples 
#' n_age_init <- 50   # Initial age of cohort in trial
#' trial_time <- 5    # Length of clinial trial in years
#' ohr        <- 0.55 # Overall hazard ratio (oHR)
#' mu_Dis     <- 0.05 # Disease-specific mortality rate
#' \dontrun{
#'    dshr_emp <- calc_dshr_from_ohr(ohr = ohr, mu_Dis = mu_Dis,
#'    n_age_init = n_age_init, 
#'    trial_time = trial_time, 
#'    hazard = "empirical")
#'    dshr_emp
#' }
calc_ohr_from_dshr <- function(dshr = 0.5, mu_Dis = 0.05, 
                               n_age_init = 50, trial_time = 5,
                               mu0 = NULL, alpha = NULL,
                               hazard = c("empirical", "exponential", "linear", "geometric"), 
                               mortality = NULL){
  ### Sanity checks
  ## Check that `hazard` contains valid value
  hazard <- match.arg(hazard)
  ## Check that mu0 and alpha are supplied when not empirical
  if(hazard %in% c("exponential", "linear", "geometric")){
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
  ## Average background mortality for the trial over trial_time years for a 
  ## cohort with initial age n_age_init,
  ohr <- (dshr*mu_Dis + lambda_b_hat)/(lambda_b_hat + mu_Dis)
  return(ohr)
}
