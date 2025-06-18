# load packages
pacman::p_load(dplyr, tidyr, readr, readxl)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Emails to authors/Georgia data")

# load raw data
georgia_raw <- read_excel("data/Study information.xlsx")

# keep columns of interest
georgia_clean <- georgia_raw %>%
select(
  "Lab number", 
  "Timeframe", 
  "Hepatitis", 
  "Lab tests date", 
  "HIV lab test result", 
  "HCV  lab test result", 
  "First time drug injection", 
  "Number shared drugs", 
  "Frequency of drug use then", 
  "Sexual experience ever", 
  "Anal oral sex contacts male", 
  "Number bisexual male sex partners", 
  "Number sex partners you take money for Sex"
)

# rename columns
colnames(georgia_clean) <- c(
  "study_id", 
  "timeframe", 
  "hcv_baseline", 
  "lab_tests_date", 
  "hiv_test_reslt", 
  "hcv_test_reslt", 
  "age_first_injected", 
  "number_shared_syringes_6m", 
  "inject_freq_6m", 
  "sexual_experience_ever", 
  "anal_oral_male_2y",  
  "number_bisexual_male_2y", 
  "sw_sell_2y"
)