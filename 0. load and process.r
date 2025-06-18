# load packages
pacman::p_load(dplyr, tidyr, readr, readxl, lubridate)

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
  "lab_test_dte", 
  "hiv_test_rslt", 
  "hcv_test_rslt", 
  "age_first_injected", 
  "number_shared_syringes_6m", 
  "inject_freq_6m", 
  "sexual_experience_ever", 
  "anal_oral_male_2y",  
  "number_bisexual_male_2y", 
  "sw_sell_2y"
)

# For individual counts of each variable
summary_table <- sapply(
  georgia_clean[, c("hcv_baseline", "hiv_test_rslt", "hcv_test_rslt", "anal_oral_male_2y", "sw_sell_2y")],
  table,
  useNA = "ifany"
)

# Print the summary table
summary_table

# convert lab_test_dte to Date format
georgia_clean <- georgia_clean %>%
  mutate(lab_test_dte = as.Date(lab_test_dte, format = "%b %d, %Y")) %>%
  arrange(study_id, lab_test_dte)

# HIV testing data
georgia_hiv_incidence <- georgia_clean %>%
  select(study_id, lab_test_dte, hiv_test_rslt, anal_oral_male_2y, number_bisexual_male_2y, sw_sell_2y)

# recode hiv_test_reslt
georgia_hiv_incidence$hiv_test_rslt <- dplyr::recode(
  georgia_hiv_incidence$hiv_test_rslt,
  "No" = "Negative",
  "Excluded" = NA_character_
)

# drop if hiv_test_reslt is NA
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  filter(!is.na(hiv_test_rslt))

# table of hiv_test_reslt
hiv_test_table <- table(georgia_hiv_incidence$hiv_test_rslt, useNA = "ifany")
hiv_test_table

# create sequence id for each study_id and remove ids with only one test
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  arrange(study_id, lab_test_dte) %>%
  group_by(study_id) %>%
  mutate(
    id_seq = row_number(),
    id_seq_max = n()
  ) %>%
  ungroup() %>%
  filter(id_seq_max > 1)

# table of hiv_test_reslt
hiv_test_table <- table(georgia_hiv_incidence$hiv_test_rslt, useNA = "ifany")
hiv_test_table ## 5 positives

# lag hiv_test_reslt and lab_test_dte
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  mutate(
    hiv_test_rslt_lag = lead(hiv_test_rslt),
    lab_test_dte_lag = lead(lab_test_dte)
  )

# days at risk
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  mutate(days_risk = as.numeric(lab_test_dte_lag - lab_test_dte))

# look at data
positive_ids <- georgia_hiv_incidence %>%
  filter(hiv_test_rslt == "Positive") %>%
  pull(study_id) %>%
  unique()

all_positive_data <- georgia_hiv_incidence %>%
  filter(study_id %in% positive_ids)

View(all_positive_data)

# remove nth row of each study_id
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  group_by(study_id) %>%
  filter(row_number() != n()) %>%
  ungroup()

# msm exposure
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  mutate(any_msm_2y = ifelse(anal_oral_male_2y == "Anal, oral", 1, 0)) 
 
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  mutate(
    any_msm_2y = ifelse(any_msm_2y == 1, 1,
                        ifelse(number_bisexual_male_2y >= 1, 1, 0))
  )

# sex work exposure
georgia_hiv_incidence <- georgia_hiv_incidence %>%
  mutate(
    sw_sell_2y = case_when(
      sw_sell_2y == "No" ~ 0,
      as.numeric(sw_sell_2y) > 1 ~ 1,
      TRUE ~ as.numeric(sw_sell_2y)
    )
  )

# look at data
View(georgia_hiv_incidence)

# table of hiv_test_reslt
hiv_test_table <- table(georgia_hiv_incidence$hiv_test_rslt_lag, useNA = "ifany")
hiv_test_table

# save HIV data
write_csv(georgia_hiv_incidence, "data/georgia_hiv_incidence.csv")