#### Preamble ####
# Purpose: Testing datasets
# Author: Boxuan Yi, Ruoxian Wu


library(haven)

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
