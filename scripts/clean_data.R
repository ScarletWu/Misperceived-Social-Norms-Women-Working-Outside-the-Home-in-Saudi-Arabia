# Load necessary packages
library(dplyr)
library(haven)

# Assuming datadir is defined, replace it with your actual directory path
datadir <- "/cloud/project/inputs/data/"
raw_data_path <- file.path(datadir, "raw_data/01_main_exp_raw1.dta")

# Read the data file
df <- read_dta(raw_data_path)

# Rename variablesdf

df <- df %>%
  rename(
    labor_demand_guess = q45_1_p2
  ) %>%
  # Generate or replace variables based on conditions
  mutate(
    labor_demand_guess = ifelse(is.na(labor_demand_guess), q45_4_p2, labor_demand_guess),
    labor_demand_guess = ifelse(is.na(labor_demand_guess), q46_1_p2, labor_demand_guess),
    labor_demand_guess = ifelse(is.na(labor_demand_guess), q43_1_p2, labor_demand_guess),
    signed_up_number = ifelse(is.na(signed_up), 0, signed_up),
    signed_up_number = ifelse(no_wife_number == 1, 0, signed_up_number),
    condition = ifelse(is.na(condition), 0, condition),
    age = ifelse(age < 18 | age > 36, NA, age),
    college_deg = as.integer(education >= 6),
    married = as.integer(marital == 2),
    num_know_per = num_know / 30,
    num_mfs_per = num_mutual_friends / 30
  )

# Handling `stag` and loop operations for `statements`
df$stag <- !duplicated(df$session)

statements <- c("outside", "semiseg", "mwage")
for(q in statements) {
  df[[paste0(q, "_guess")]] <- df[[paste0(q, "_others")]] + df[[paste0(q, "_self")]]
  df <- df %>%
    group_by(session) %>%
    mutate(!!paste0(q, "_objective") := mean(!!sym(paste0(q, "_self")), na.rm = TRUE) * 30,
           !!paste0(q, "_wedge") := !!sym(paste0(q, "_guess")) - !!sym(paste0(q, "_objective")))
  df[[paste0(q, "_wedge_pos")]] <- as.integer(df[[paste0(q, "_wedge")]] > 0)
}

df$interaction <- with(df, condition2 * outside_wedge_pos)

# Dropping irrelevant variables
df <- df %>%
  select(-condition, -marital, -employed_ever, -num_know, -num_mutual_friends, -q45_4_p2, -q46_1_p2, -q43_1_p2, -count, -condition_txt, -glowork_choice, -second_order_total, -signed_up, -no_wife_number)

# Save the cleaned data
clean_data_path <- file.path(datadir, "clean_data/01_main_exp_clean.dta")
write_dta(df, clean_data_path)
