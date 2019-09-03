#' Disease-specific hazard ration (dsHR) from an overall hazard ratio (oHR)
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

#' Average background mortality 
#' 
#' \code{calc_avg_mort} calculates the average background mortality for a
#' cohort with initial age \code{n_age_init} in a trial with duration of
#' \code{trial_time} years for either empirical hazard (e.g.. from life tables) 
#' or some continuous-time hazard functional forms.
#' @param n_age_init initial age of the cohort in the trial
#' @param trial_time length of the trial
#' @param mu0 age-independent mortality rate coefficient
#' @param alpha age-dependent mortality rate coefficient
#' @param hazard The hazard function to be used for background mortality. 
#' Either empirical (e.g., from life tables) or a continuous-time functional 
#' form.
#' @param mortality A \code{data.frame} with two columns. The first column 
#' should have age in yearly increments and the second column should have 
#' the mortality rate corresponding to the age. If \code{NULL}, the function
#' will use \code{\link{all_cause_mortality}}.
#' @return 
#' The average background mortality
#' @export
#' 
#' @examples 
#' n_age_init <- 50   # Initial age of cohort in trial
#' trial_time <- 5    # Length of clinial trial in years
#' \dontrun{
#'    dshr_emp <- calc_avg_mort(n_age_init = n_age_init, 
#'    trial_time = trial_time, 
#'    hazard = "empirical")
#'    dshr_emp
#' }
calc_avg_mort <- function(n_age_init = 50, trial_time = 5,
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
  ## Check if data is NULL
  if(is.null(mortality)){
    mortality <- all_cause_mortality[, c("Age", "Total")]
  } else if(!is.data.frame(mortality)){
    stop("`mortality` must be a data.frame")
  } else if(ncol(mortality)!=2){
    stop("`mortality` must have two columns: 1) Age, 2) mortality rate")
  }
  ## Age of cohort at the end of the trial
  n_age_max  <- n_age_init + trial_time
  ## Extract mortality for specified ages
  colnames(mortality) <- c("Age", "Hazard")
  mortality <- mortality %>% 
    dplyr::filter(.data$Age>=n_age_init & .data$Age<n_age_max)
  lambda_b_hat <- switch(hazard,
    "empirical"   = mean(mortality$Hazard),
    "exponential" = mu0*exp(alpha*n_age_init)*(exp(alpha*trial_time) - 1)/(alpha * trial_time),
    "linear"      = mu0 + 0.5*alpha*(2*n_age_init + trial_time),
    "geometric"   = mu0*(1+alpha)^n_age_init*((1 + alpha)^trial_time - 1)/(log(1+alpha)*trial_time)
  )
  return(lambda_b_hat)
}

#' Estimation of hazard functional forms for background mortality 
#' 
#' \code{est_hazard_params} estimates the parameters of different 
#' continuous-time hazard functional forms for background  mortality for a 
#' cohort with initial age \code{n_age_init} in a trial with duration of 
#' \code{trial_time} years.
#' 
#' @param n_age_init Initial age of the cohort
#' @param trial_time Duration of clinical trial in years
#' @param hazard The hazard function to be used for background mortality. 
#' Either empirical (e.g., from life tables) or a continuous-time functional 
#' form.
#' @param mortality A \code{data.frame} with two columns. The first column 
#' should have age in yearly increments and the second column should have 
#' the mortality rate corresponding to the age. If \code{NULL}, the function
#' will use \code{\link{all_cause_mortality}}.
#' @return 
#' A vector with an age-independent mortality rate coefficient, \code{mu0} and
#' an age-dependent mortality rate coefficient, \code{alpha}.
#' @export
#' 
#' @examples 
#' n_age_init <- 50   # Initial age of cohort in trial
#' trial_time <- 5    # Length of clinial trial in years
#' \dontrun{
#'    v_coef_hazard <- est_hazard_params(n_age_init = n_age_init, 
#'    trial_time = trial_time, 
#'    hazard = "exponential")
#'    v_coef_hazard
#' }
est_hazard_params <- function(n_age_init = 50, trial_time = 5,
                              hazard = c("exponential", "linear", "geometric"), 
                              mortality = NULL){
  ### Sanity checks
  ## Check that `hazard` contains valid value
  hazard <- match.arg(hazard)
  ## Check if data is NULL
  if(is.null(mortality)){
    mortality <- all_cause_mortality[, c("Age", "Total")]
  } else if(!is.data.frame(mortality)){
    stop("`mortality` must be a data.frame")
  } else if(ncol(mortality)!=2){
    stop("`mortality` must have two columns: 1) Age, 2) mortality rate")
  }
  
  ### Function implementation
  ## Age of cohort at the end of the trial
  n_age_max  <- n_age_init + trial_time
  ## Extract mortality for specified ages
  colnames(mortality) <- c("Age", "Hazard")
  mortality <- mortality %>% 
    dplyr::filter(.data$Age>=n_age_init & .data$Age<=n_age_max)
  ## Estimate parameters of selected hazard functional form
  if(hazard=="exponential"){
    fit_hazard <- lm(log(Hazard) ~ Age, 
                     data = mortality)
    coef_fit <- coef(fit_hazard)
    mu0   <- exp(coef_fit[1])
    alpha <- coef_fit[2]
  } else if (hazard=="linear"){
    fit_hazard <- lm(Hazard ~ Age, 
                     data = mortality)
    coef_fit <- coef(fit_hazard)
    mu0   <- coef_fit[1]
    alpha <- coef_fit[2]
  } else if (hazard=="geometric"){
    fit_hazard <- lm(log(Hazard) ~ Age, 
                     data = mortality)
    coef_fit <- coef(fit_hazard)
    mu0   <- exp(coef_fit[1])
    alpha <- exp(coef_fit[2])-1
  }
  coef_hazard <- list(mu0 = mu0, alpha = alpha)
  names(coef_hazard) <- c("mu0", "alpha")
  return(coef_hazard)
}
