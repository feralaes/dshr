
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![DOI](https://zenodo.org/badge/205402838.svg)](https://zenodo.org/badge/latestdoi/205402838)

dshr
====

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/feralaes/dshr.svg?branch=master)](https://travis-ci.org/feralaes/dshr) <!-- badges: end -->

An R package that implements the methods to derive disease-specific hazard ratios (dsHR) from overall hazard ratios (oHR) described in the publication:

-   Alarid-Escudero F, Kuntz KM "Potential bias associated with modeling the effectiveness of healthcare interventions in reducing mortality using an overall hazard ratio." PharmacoEcon 2019 (Forthcoming)

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("feralaes/dshr")
```

Example
-------

This is a basic example which shows you how to derive a disease-specific hazard ratio (dsHR) from a disease-specific mortality rate, an overall hazard ratio (oHR) reported in a trial, the initial age of the cohort in the trial, and the length of the trial using an *empirical* approach with background mortality from US life tables.

``` r
library(dshr)
ohr        <- 0.55 # Overall hazard ratio (oHR)
n_age_init <- 50   # Initial age of cohort in trial
trial_time <- 5    # Length of clinial trial in years
mu_Dis     <- 0.05 # Disease-specific mortality rate

dshr_emp <- calc_dshr_from_ohr(ohr = ohr, 
                               mu_Dis = mu_Dis,
                               n_age_init = n_age_init, 
                               trial_time = trial_time, 
                               hazard = "empirical")
dshr_emp
#>      dshr 
#> 0.5038836
```

To estimate dsHR assuming an anlaytical functional form of background mortality (either exponential, linear or geometric), estimate the `mu0` and `alpha` parameters of the functional form, type them in `calc_dshr_from_ohr` and substitute `empirical` with the functional form of interest. For example, using an *exponential* functional form with background mortality from US life tables, the estimated parameters are

``` r
coef_hazard_exp <- est_hazard_params(n_age_init = n_age_init,
                                     trial_time = trial_time,
                                     hazard = "exponential")
coef_hazard_exp
#> $mu0
#>  (Intercept) 
#> 0.0001105985 
#> 
#> $alpha
#>        Age 
#> 0.07362561
```

and the dshr is

``` r
dshr_exp <- calc_dshr_from_ohr(ohr = ohr, 
                               mu_Dis = mu_Dis,
                               n_age_init = n_age_init, 
                               trial_time = trial_time, 
                               mu0 = coef_hazard_exp$mu0,
                               alpha = coef_hazard_exp$alpha,
                               hazard = "exponential")
dshr_exp
#>      dshr 
#> 0.5022318
```
