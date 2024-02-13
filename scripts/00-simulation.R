#### Preamble ####
# Purpose: Simulating data on survery results 
# Authors: Boxuan Yi, Ruoxian Wu
# Pre-requisites: none

# Install packages we need
install.packages('haven')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('readr')
install.packages('here')
install.packages('janitor')
install.packages('knitr')
install.packages('kableExtra')
install.packages('tidyverse')

# Load Packages
library(tidyverse)
library(janitor)
library(knitr)

# Simulation for three surveys
# Simulation for the main experiment
set.seed(1232)
condition <- c("0", "1")
simulated_main_exp <-
  tibble("Condition2" = sample(condition, 500, replace = TRUE),
         "Age" = sample(18:35, 500, replace = TRUE),
         "College_deg" = sample(0:1, 500, replace = TRUE),
         "employed_now" = sample(0:1, 500, replace = TRUE),
         "employed_wife" = sample(0:1, 500, replace = TRUE),
         "children" = sample(0:12, 500, replace = TRUE),
         "mwage_self" = sample(0:1, 500, replace = TRUE),
         "mwage_others" = sample(0:29, 500, replace = TRUE),
         "mwage_confidence" = sample(1:5, 500, replace = TRUE),
         "outside_self" = sample(0:1, 500, replace = TRUE),
         "outside_others" = sample(0:29, 500, replace = TRUE),
         "outside_confidence" = sample(1:5, 500, replace = TRUE),
         "semiseg_self" = sample(0:1, 500, replace = TRUE),
         "semiseg_others" = sample(0:29, 500, replace = TRUE), 
         "semiseg_confidence" = sample(1:5, 500, replace = TRUE),
         'labor_demand_guess' = sample(0:100, 500, replace = TRUE),
         'signed_up_number' = sample(0:1, 500, replace = TRUE),
         'outside_guess' = sample(0:30, 500, replace = TRUE),
         'semiseg_guess' = sample(0:30, 500, replace = TRUE),
         'mwage_guess' = sample(0:30, 500, replace = TRUE)) |> kable()
simulated_main_exp

# Simulation for the follow-up
simulated_follow_up <-
  tibble(
    "Condition2" = sample(condition, 500, replace = TRUE),
    "Age" = sample(18:35, 500, replace = TRUE),
    "College_deg" = sample(0:1, 500, replace = TRUE),
    "employed_now" = sample(0:1, 500, replace = TRUE),
    "employed_wife" = sample(0:1, 500, replace = TRUE),
    "children" = sample(0:12, 500, replace = TRUE),
    "outside_self" = sample(0:1, 500, replace = TRUE),
    "outside_others" = sample(0:29, 500, replace = TRUE),
    "outside_confidence" = sample(1:5, 500, replace = TRUE),
    "semiseg_self" = sample(0:1, 500, replace = TRUE),
    "semiseg_others" = sample(0:29, 500, replace = TRUE), 
    'employed_3mos_out_fl' = sample(0:1, 500, replace = TRUE),
    'employed_now_out_fl' = sample(0:1, 500, replace = TRUE),
    'applied_out_fl' = sample(0:1, 500, replace = TRUE),
    "interviewed_out_fl" = sample(0:1, 500, replace = TRUE),
    "interview_sched_out_fl" = sample(0:1, 500, replace = TRUE),
    'driving_fl' = sample(0:1, 500, replace = TRUE),
    'outside_others_fl' = sample(0:29, 500, replace = TRUE), 
    'matched' = sample(0:1, 500, replace = TRUE),
    'signed_up_number' = sample(0:1, 500, replace = TRUE),
    'outside_guess' = sample(0:30, 500, replace = TRUE)) |> kable()
simulated_follow_up

# Simulation for the online survey
simulated_online_survey <-
  tibble("Condition" = sample(condition, 1460, replace = TRUE),
         "Age" = sample(18:35, 1460, replace = TRUE),
         "College_deg" = sample(0:1, 1460, replace = TRUE),
         "employed_now" = sample(0:1, 1460, replace = TRUE),
         "employed_wife" = sample(0:1, 1460, replace = TRUE),
         "employed_out_wife" = ifelse(employed_wife == 1, sample(0:1, sum(employed_wife == 1), replace = TRUE), 0),
         "children" = sample(0:12, 1460, replace = TRUE),
         'c_outside_self' = ifelse(Condition == 0, sample(0:1, sum(Condition == 0), replace = TRUE), 0),
         'c_outside_guess' = ifelse(Condition == 0, runif(n = sum(Condition == 0), min = 0, max = 100), 0),
         'c_outside_confidence' = ifelse(Condition == 0, sample(1:5, sum(Condition == 0), replace = TRUE), 0),
         't_outside_guess' = ifelse(Condition == 1, runif(n = sum(Condition == 1), min = 0, max = 100), 0),
         't_outside_confidence' = ifelse(Condition == 1, sample(1:5, sum(Condition == 0), replace = TRUE), 0)
  ) |> kable()
simulated_online_survey
