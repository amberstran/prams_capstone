#### BIS 560 Final Paper -- PRAMS Phase 8 Dataset Cleaning ###
## Authors: Nat Chairuengjitjaras & Amber Tran
## Date: Nov 2024

# Install and load libraries
library(haven)
library(dplyr)

# Read in dataset
prams_dat <- read_sas("/Users/ambertran/Documents/Yale_MS_Health_Informatics/BIS_560/phase8_arf_2016_2022.sas7bdat")

## Analysis 1: Predictive factors for BC
## Population of interest: respondents who answered Q43 (BC_NOW4) {Are you on birth control now?}
## Remove Vermont and Alaska from analysis, different demographics
## Variables of interests: Education, Age Group, Race/Ethnicity, Total Household Income, Insurance, Marital status, State
## Insurance: Re-code to 4 factor variable indicating type of coverage {0 = public, 1 = private, 2 = both, 3 = no insurance}

#Demographics includes state of birth, household income, marital status, age group, maternal highest degree, maternal race/ethnicity
dems <- c('ID','STATE','INCOME8','MARRIED','MAT_AGE_PU','MAT_DEG','MAT_RACE_PU', 'HISP_BC')

# Postpartum Insurance Coverage
pp_ins_vars <- c('PP8_HCEX', 'PP8_PAR', 'PP8_WORK', 'PP_CHIP', 'PP_GOV', 'PP_GOV2', 'PP_IHS', 'PP_MEDIC', 'PP_MILIT', 'PP_NONE')

# Reasons/barriers for no BC
BC_barr_vars <- c('BC_NOW4', 'BCB_PREG_RAW', 'BCB_PNOW_RAW', 'BCB_TUBE_RAW', 'BCB_WANT_RAW', 'BCB_SIDE_RAW', 'BCB_NSEX_RAW', 'BCB_HUSB_RAW', 'BCB_PAY_RAW')

prams_dat_clean <- prams_dat %>%
  filter(BC_NOW4 != "B") %>%
  filter(!(STATE %in% c("AK", "VT"))) %>%
  select(all_of(dems), all_of(pp_ins_vars), all_of(BC_barr_vars))

# Re-code insurance variables
prams_dat_clean <- prams_dat_clean %>%
  mutate(
    # Determine if private insurance is present
    ins_priv = case_when(
      PP8_HCEX == 2 | PP8_PAR == 2 | PP8_WORK == 2 ~ 1,
      is.na(PP8_HCEX) & is.na(PP8_PAR) & is.na(PP8_WORK) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Determine if public insurance is present
    ins_gov = case_when(
      PP_CHIP == 2 | PP_GOV == 2 | PP_GOV2 == 2 | PP_IHS == 2 | PP_MEDIC == 2 | PP_MILIT == 2 ~ 1,
      is.na(PP_CHIP) & is.na(PP_GOV) & is.na(PP_GOV2) & is.na(PP_IHS) & is.na(PP_MEDIC) & is.na(PP_MILIT) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Determine the insurance type
    PP_INS_TYPE = case_when(
      # If both private and public insurance are present, assign 2
      ins_priv == 1 & ins_gov == 1 ~ 2,
      # If only private insurance, assign 1
      ins_priv == 1 & ins_gov == 0 ~ 1,
      # If only public insurance, assign 0
      ins_gov == 1 & ins_priv == 0 ~ 0,
      # If PP_NONE == 2 and no private or public insurance, assign 3
      PP_NONE == 2 & ins_priv == 0 & ins_gov == 0 ~ 3,
      # For missing or undefined cases, assign NA
      TRUE ~ NA_real_
    )
  ) %>%
  # Re-code to standard binary 0=no, 1=yes
  mutate(BC_NOW4 = ifelse(BC_NOW4 == 1, 0, 1))

# Dataset 1: Demographics + Insurance Type Only
prams_dat_1_final <- prams_dat_clean %>%
  select(all_of(dems), 'PP_INS_TYPE', 'BC_NOW4') %>%
  na.omit() %>%
  mutate(across(everything(), ~ {attr(.x, "label") <- NULL; .x}))

## Analysis 2: Variation of reasons for no BC across states
## Population of interest: respondents who answered NO for Q43 (BC_NOW4) {Are you on birth control now?}
## Remove Vermont and Alaska from analysis, different demographics
## Variables of interests: Education, Age Group, Race/Ethnicity, Total Household Income, Insurance, Marital status, State
## Insurance: Re-code to 4 factor variable indicating type of coverage {0 = public, 1 = private, 2 = both, 3 = no insurance}

# Dataset 2: Demographics + Insurance Type + Reasons/Barriers for no BC
prams_dat_2_final <- prams_dat_clean %>%
  filter(BC_NOW4 == 0) %>%
  select(all_of(dems), 'PP_INS_TYPE', all_of(BC_barr_vars)) %>%
  na.omit() %>%
  mutate(across(everything(), ~ {attr(.x, "label") <- NULL; .x}))


# Export csv files
write.csv(prams_dat_1_final, '/Users/ambertran/Documents/Yale_MS_Health_Informatics/BIS_560/PRAMS_Dataset1_Clean.csv')
write.csv(prams_dat_2_final, '/Users/ambertran/Documents/Yale_MS_Health_Informatics/BIS_560/PRAMS_Dataset2_Clean.csv')



