#### Preamble ####
# Purpose: Clean the raw dataset
# Author: Boxuan Yi, Ruoxian Wu

library(haven)
library(dplyr)

# Define the directory paths
datadir <- "/cloud/project/inputs/data/"
raw_data_path <- file.path(datadir, "raw_data/01_main_exp_raw2.dta")

# Load the main experiment dataset
df_main_exp <- read_dta(raw_data_path)

# Adjust 'session' based on specific 'digits' values
df_main_exp <- df_main_exp %>%
  mutate(dup_index = row_number() + 1000) %>%
  mutate(session = case_when(
    session == 1 & digits == 136 ~ dup_index,
    session == 5 & digits == 430 ~ dup_index,
    session == 6 & digits == 651 ~ dup_index,
    session == 6 & digits == 922 ~ dup_index,
    session == 7 & digits == 646 ~ dup_index,
    session == 9 & digits == 16 ~ dup_index,
    session == 9 & digits == 449 ~ dup_index,
    session == 12 & digits == 282 ~ dup_index,
    session == 13 & digits == 880 ~ dup_index,
    session == 15 & digits == 84 ~ dup_index,
    session == 17 & digits == 341 ~ dup_index,
    session == 17 & digits == 966 ~ dup_index,
    TRUE ~ as.double(session)
  ))

# Load the follow-up dataset
follow_up_path1 <- file.path(datadir, "raw_data/02_follow_up_raw1.dta")
df_follow_up1 <- read_dta(follow_up_path1)

# Merge the main experiment and follow-up datasets
df_merged <- df_main_exp %>%
  left_join(df_follow_up1, by = c("session", "digits"))

# Flag matched rows based on 'work_outside_1_fl' being non-NA
df_merged <- df_merged %>%
  mutate(matched = if_else(!is.na(work_outside_1_fl), 1, 0))

# Recode follow-up variables and perform further data manipulation
df_clean <- df_merged %>%
  mutate(across(starts_with("employed_3mos_fl"):starts_with("driving_fl"), ~if_else(. == 2, 0, .))) %>%
  rename(outside_others_fl = work_outside_1_fl) %>%
  mutate(
    outside_others_fl2 = outside_others_fl / 30,
    outside_others_2 = outside_others / 30,
    min_wage_fl_per = min_wage_1_fl / 30
  )

# Select relevant variables for the final cleaned dataset
df_final <- df_clean %>%
  select(
    condition, digits, age, marital, education, employed_ever, employed_now, employed_wife, 
    children, num_know, num_mutual_friends, haafez_self, vjobs_self, mwage_self, 
    mwage_others, mwage_confidence, outside_self, outside_others, outside_confidence, 
    semiseg_self, semiseg_others, semiseg_confidence, signed_up, employed_3mos_fl, 
    employed_3mos_out_fl, employed_now_fl, employed_now_out_fl, applied_fl, applied_out_fl, 
    interviewed_fl, interviewed_out_fl, interview_sched_fl, interview_sched_out_fl, driving_fl, 
    outside_others_fl, work_semiseg_1_fl, employed_3mos_oc, employed_now_oc, matched,
    # Add derived columns
    outside_others_fl2, min_wage_fl_per
  )


# Save the cleaned dataset
clean_data_path <- file.path(datadir, "clean_data/02_follow_up_clean.dta")
write_dta(df_final, clean_data_path) # Ensure df_final is saved, not df_merged
