################################################################################
#
# Script name:          Data_processing.R
# Script description:   This script formats data from the Health Survey for 
#                       England (HSE) 2019 dataset.
#
# Author:               @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse)

# DATA PROCESSING --------------------------------------------------------------

# Download Health Survey for England (2019) dataset from project directory
# (accessed from UK Data Service, 
# https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000021)
hse19_data <- haven::read_dta("./Data/hse_2019_eul_20211006.dta")

# Define variables to extract from HSE 2019 dataset
# (format is "New_Var_Name" = "HSE_Var_Name")
vars <- c("ID" = "SerialA",                    # archive respondent serial number
          "Sample_weight" = "wt_int",          # weight for analysis of core interview sample
          "LLTI" = "limitill",                 # limiting longstanding illness
          "Optimism" = "OptimF",               # been feeling optimistic about the future
          "Income" = "eqv5",                   # equivalised income quintiles
          "Age" = "age16g5",                   # age 16+, 5-year bands
          "Sex" = "Sex",                       # sex
          "Ethnicity" = "origin2",             # grouped ethnic categories
          "Education" = "Educ2",               # highest educational qualification
          "BMI" = "BMIVal2",                   # valid BMI measurements using est. weight if >200kg
          "Drinking" = "dnoft3_19",            # frequency drank any alcoholic drink in last 12 mos.
          "Smoking" = "cigst2_19",             # cigarette smoking status - banded current smokers
          "Medication" = "medcnj",             # whether taking medication - excl. contraceptives
          "Employment" = "HRPactIv3",          # activity (employment) status for the last week
          "Therapy" = "ThCoAny",               # any type of therapy or counselling in last 12 months
          "Marital_status" = "marstatD",       # marital status including cohabitees
          "Self_assessed_health" = "GenHelf",  # self-assessed general health
          "Survey_qtr" = "Qrtint")             # quarter of year of individual interview

# Extract defined variables from HSE 2019 dataset & specify variables as factors
data <- hse19_data %>% 
  select(all_of(vars)) %>%
  mutate(across(.cols = -c(ID, Sample_weight, BMI), 
                .fns = sjlabelled::as_label))
#data

# Print value labels
data %>% 
  map(.x = ., 
      .f = ~ sjlabelled::get_labels(., values = "n"))

# Recode variables & print new value labels
data <- data %>%
  mutate(across(.cols = where(is.factor),
                .fns = ~ fct_collapse(., NULL = c("Refused", "Don't know", "Not applicable")))) %>%
  mutate(LLTI = fct_collapse(LLTI, 
                             "No" = c("No LI", "Non-limiting LI"),
                             "Yes" = "Limiting LI"),
         Optimism = fct_collapse(Optimism,
                                 "No" = c("None of the time", "Rarely", "Some of the time"),
                                 "Yes" = c("Often", "All of the time")),
         Medication = fct_collapse(Medication,
                                   "Yes" = c("Yes", "Yes, but unable to code as name of drug(s) not available")),
         Therapy = fct_collapse(Therapy,
                                "Yes" = "Mentioned",
                                "No" = "Not mentioned"),) %>%
  mutate(across(.cols = c(ID, Sample_weight, BMI),
                .fns = ~ ifelse(. < 0, NA, .))) %>%
  mutate(across(.cols = c(LLTI, Optimism, Medication, Therapy),
                .fns = ~ factor(., levels = c("No", "Yes"))))
data %>% 
  map(.x = ., 
      .f = ~ sjlabelled::get_labels(., values = "n"))

# Remove full HSE dataset and variable name key from global environment
rm(hse19_data, vars)
