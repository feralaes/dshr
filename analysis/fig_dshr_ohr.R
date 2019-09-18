############################################################################### 
# This script produces the figure comparing dsHR vs. oHR of the manuscript:   #
# - Alarid-Escudero F, Kuntz KM "Potential bias associated with modeling the  #
#   effectiveness of healthcare interventions in reducing mortality using an  #
#   overall hazard ratio" 2019 (Under review)                                 #
#                                                                             # 
# Authors:                                                                    #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>             # 
#     - Karen M. Kuntz, ScD                                                   #
############################################################################### 
# The most up-to-date code could be found in the following GitHub repository  #
# https://github.com/feralaes/dshr                                            #
###############################################################################

rm(list = ls()) # to clean the workspace

#### 01 Load packages, data and functions ####
library(ggplot2) # for plotting
devtools::load_all(".") # load dshr functions

#### 03 Construct design of experiment (DoE) ####
### Create vectors for each of the variables
v_n_age_init <- c(40, 50, 60, 70)     # Initial ages of cohort in trial
v_trial_time <- c(1, 5, 10)           # Length of clinial trial in years
v_dshr       <- seq(0, 1, length = 6) # Overall hazard ratio (oHR)
v_mu_Dis     <- seq(0.01, 0.10, length = 10) # Disease-specific mortality rate

### Generate full factorial DoE
df_doe_dshr <- expand.grid(
  n_age_init = v_n_age_init,
  trial_time = v_trial_time,
  dshr        = v_dshr,
  mu_Dis     = v_mu_Dis
)

### Number of simulations
n_sim_doe <- nrow(df_doe_dshr)

### Initialize vector to store oHR outputs
v_ohr_emp <- numeric(n_sim_doe)

#### 04 Run experiment ####
for(i in 1:n_sim_doe){
  v_ohr_emp[i] <- with(as.list(df_doe_dshr[i, ]),{
    calc_ohr_from_dshr(dshr = dshr, mu_Dis = mu_Dis,
                       n_age_init = n_age_init,
                       trial_time = trial_time,
                       hazard = "empirical")
  }
  )
}

#### 05 Format ouput ####
df_doe_ohr_vs_dshr <- data.frame(df_doe_dshr, 
                                 ohr = v_ohr_emp)
df_doe_ohr_vs_dshr$Age <- as.factor(df_doe_ohr_vs_dshr$n_age_init)
levels(df_doe_ohr_vs_dshr$Age) <- c("Cohort age = 40 yrs",
                                    "Cohort age = 50 yrs", 
                                    "Cohort age = 60 yrs", 
                                    "Cohort age = 70 yrs")
df_doe_ohr_vs_dshr$`Trial years` <- as.factor(df_doe_ohr_vs_dshr$trial_time)
levels(df_doe_ohr_vs_dshr$`Trial years`) <- c("Trial time = 1 yr",
                                              "Trial time = 5 yrs", 
                                              "Trial time = 10 yrs")
#### 06 Generate plot ####
ggplot(df_doe_ohr_vs_dshr, aes(x = ohr, 
                               y = dshr, 
                               group = mu_Dis, color = mu_Dis)) +
  geom_line() + 
  facet_grid(`Trial years` ~ Age) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  # scale_colour_colorblind() +
  # scale_colour_continuous(trans = "reverse") +
  # scale_color_gradient(low = "red", high = "darkgreen") +
  scale_color_gradientn(colours = rev(c("#1E9600","#F9EB18","#F98020","#F51329","#FF0000"))) +
  xlab("oHR") + 
  ylab("dsHR") + 
  labs(color = expression(mu[Dis])) +
  theme_bw(base_size = 16) +
  theme(panel.spacing = unit(1, "lines")) +
  NULL
ggsave("inst/extdata/dshr_vs_ohr.pdf", width = 14, height = 8)

# ggplot(df_doe_ohr_vs_dshr, aes(x = ohr, 
#                    y = dshr, 
#                    group = mu_Dis, color = mu_Dis)) +
#   geom_line() + 
#   facet_grid(Age ~ `Trial years`) +
#   scale_y_continuous(limits = c(0, 1)) +
#   geom_abline(slope = 1, intercept = 0, linetype = 2) +
#   # scale_colour_colorblind() +
#   # scale_colour_continuous(trans = "reverse") +
#   # scale_color_gradient(low = "red", high = "darkgreen") +
#   scale_color_gradientn(colours = rev(c("#1E9600","#F9EB18","#F98020","#F51329","#FF0000"))) +
#   xlab("oHR") + 
#   ylab("dsHR") + 
#   labs(color = expression(mu[Dis])) +
#   # scale_color_viridis(discrete = FALSE, direction = -1) +
#   # scale_color_viridis(discrete = FALSE, direction = 1, option = "A") +
#   # scale_color_viridis(discrete = FALSE, direction = 1, option = "E") +
#   theme_bw(base_size = 14) +
#   theme(panel.spacing = unit(1, "lines")) +
#   NULL