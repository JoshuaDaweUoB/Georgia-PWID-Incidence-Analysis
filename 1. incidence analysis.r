# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Emails to authors/Georgia data")

# load hiv data
georgia_hiv_incidence <- read_excel("data/georgia_hiv_incidence.csv")

# Count incident HIV cases (assuming each "Positive" is an incident case)
num_incident_cases <- sum(georgia_hiv_incidence$hiv_test_rslt == "Positive", na.rm = TRUE)

# Calculate total person-time in years
total_person_years <- sum(georgia_hiv_incidence$days_risk, na.rm = TRUE) / 365.25

# Incidence rate per 100 person-years
incidence_rate <- (num_incident_cases / total_person_years) * 100

incidence_rate