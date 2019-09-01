## code to prepare `01_all_cause_mortality` dataset goes here
all_cause_mortality <- readxl::read_xlsx("data-raw/US_LifeTable09_rates.xlsx", sheet = 2)

colnames(all_cause_mortality) <- c("Age", "Total", "Male", "Female")

# Create .rda object for initial set of parameters and store it in 'data' folder
usethis::use_data(all_cause_mortality, overwrite = TRUE)
