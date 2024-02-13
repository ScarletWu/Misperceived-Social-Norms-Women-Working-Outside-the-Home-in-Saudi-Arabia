#### Preamble ####
# Purpose: Testing datasets
# Author: Boxuan Yi, Ruoxian Wu

library(dplyr)
library(haven)
## Clean data test
main_clean <- read_dta(here("inputs/data/01_main_exp_clean_original.dta"))
follow_up_clean <- read_dta(here("inputs/data/02_follow_up_clean_original.dta"))
online_survey_clean <- read_dta(here("inputs/data/03_1st_online_svy_clean_original.dta"))

# Main experiment dataset tests
length(unique(main_clean$condition2)) == 2
main_clean$age |> unique()
main_clean$education |> max() <= 9
main_clean$education |> min() >= 1
main_clean$outside_confidence |> max() <= 5
main_clean$outside_wedge |> min() >= -29
main_clean$outside_wedge |> min() <= 29
length(unique(main_clean$signed_up_number)) == 2
length(unique(main_clean$employed_wife)) == 2

# Follow-up tests
length(unique(follow_up_clean$employed_now)) == 2
length(unique(follow_up_clean$college_deg)) == 2
follow_up_clean |> filter(employed_wife == 1) |> nrow()

wife_outside3mos_num <- follow_up_clean |> filter(employed_3mos_out_fl == 1) |> nrow()
wife_3mos_num <- follow_up_clean |> filter(employed_wife == 1) |> nrow()
wife_outside3mos_num <= wife_3mos_num

# Online survey tests
online_survey_clean |> filter(condition == 0) |> nrow()
online_survey_clean |> filter(!is.na(c_list)) |> nrow()
online_survey_clean |> filter(!is.na(c_outside_self)) |> nrow()
online_survey_clean |> filter(!is.na(c_outside_wedge)) |> nrow()

online_survey_clean |> filter(condition == 1) |> nrow()
online_survey_clean |> filter(!is.na(t_outside_confidence)) |> nrow()
online_survey_clean |> filter(!is.na(t_outside_guess_frac)) |> nrow()
online_survey_clean |> filter(!is.na(t_outside_guess)) |> nrow()


## Raw data test

# Data path
datadir <- "/cloud/project/inputs/data/"
raw_data_path <- file.path(datadir, "raw_data/01_main_exp_raw1.dta")

# Read the data file
df <- read_dta(raw_data_path)


## Missing data test

# Calculate the number of missing values per column
missing_values <- sapply(df, function(x) sum(is.na(x)))

# Print missing values
print(missing_values)

## Distribution Test for Key Variables

# Numerical variables
numerical_stats <- summary(df[c("employed_ever", "employed_now", "employed_wife", "num_mutual_friends")])

print(numerical_stats)

# Categorical variables
marital_distribution <- table(df$marital)
education_distribution <- table(df$education)
employed_ever_distribution <- table(df$employed_ever)

print(marital_distribution)
print(education_distribution)
print(employed_ever_distribution)

## clean data test

# Data path
datadir_clean <- "/cloud/project/inputs/data/"
clean_data_path <- file.path(datadir, "clean_data/01_main_exp_clean.dta")

# Read the data file
df2 <- read_dta(clean_data_path)


## Missing data test

# Calculate the number of missing values per column
missing_values <- sapply(df2, function(x) sum(is.na(x)))

# Print missing values
print(missing_values)

## Distribution Test for Key Variables

# Numerical variables
numerical_stats <- summary(df2[c("employed_now", "employed_wife", "labor_demand_guess", "signed_up_number")])

print(numerical_stats)

# Categorical variables
marital_distribution <- table(df$marital)
education_distribution <- table(df$education)
employed_ever_distribution <- table(df$employed_ever)

print(marital_distribution)
print(education_distribution)
print(employed_ever_distribution)
