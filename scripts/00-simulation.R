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
  tibble("condition2" = sample(condition, 500, replace = TRUE),
         "age" = sample(18:35, 500, replace = TRUE),
         "college_deg" = sample(0:1, 500, replace = TRUE),
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
         'mwage_guess' = sample(0:30, 500, replace = TRUE),
         'outside_wedge' = sample(-29:29, 500, replace = TRUE))
simulated_main_exp

# Simulation for the follow-up
simulated_follow_up <-
  tibble(
    "condition2" = sample(condition, 500, replace = TRUE),
    "age" = sample(18:35, 500, replace = TRUE),
    "college_deg" = sample(0:1, 500, replace = TRUE),
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
    "interviewed_out_fl" = ifelse(applied_out_fl == 1, sample(0:1, sum(applied_out_fl == 1), replace = TRUE), 0),
    "interview_sched_out_fl" = ifelse(applied_out_fl == 1, sample(0:1, sum(applied_out_fl == 1), replace = TRUE), 0),
    'driving_fl' = sample(0:1, 500, replace = TRUE),
    'outside_others_fl' = sample(0:29, 500, replace = TRUE), 
    'matched' = sample(0:1, 500, replace = TRUE),
    'signed_up_number' = sample(0:1, 500, replace = TRUE),
    'outside_guess' = sample(0:30, 500, replace = TRUE))
simulated_follow_up

# Simulation for the online survey
simulated_online_survey <-
  tibble("condition" = sample(condition, 1460, replace = TRUE),
         "age" = sample(18:35, 1460, replace = TRUE),
         "college_deg" = sample(0:1, 1460, replace = TRUE),
         "employed_now" = sample(0:1, 1460, replace = TRUE),
         "employed_wife" = sample(0:1, 1460, replace = TRUE),
         "employed_out_wife" = ifelse(employed_wife == 1, sample(0:1, sum(employed_wife == 1), replace = TRUE), 0),
         "children" = sample(0:12, 1460, replace = TRUE),
         'c_outside_self' = ifelse(condition == 0, sample(0:1, sum(condition == 0), replace = TRUE), 0),
         'c_outside_guess' = ifelse(condition == 0, runif(n = sum(condition == 0), min = 0, max = 100), 0),
         'c_outside_confidence' = ifelse(condition == 0, sample(1:5, sum(condition == 0), replace = TRUE), 0),
         't_outside_guess' = ifelse(condition == 1, runif(n = sum(condition == 1), min = 0, max = 100), 0),
         't_outside_confidence' = ifelse(condition == 1, sample(1:5, sum(condition == 0), replace = TRUE), 0)
  )
simulated_online_survey

# Tests
# Main experiment dataset tests
length(unique(simulated_main_exp$condition2)) == 2
simulated_main_exp$age |> max() <= 35
simulated_main_exp$age |> min() >= 18
length(unique(simulated_main_exp$college_deg)) == 2
simulated_main_exp$outside_confidence |> max() <= 5
simulated_main_exp$outside_wedge |> min() >= -29
simulated_main_exp$outside_wedge |> max() <= 29
length(unique(simulated_main_exp$signed_up_number)) == 2
length(unique(simulated_main_exp$employed_wife)) == 2

# Follow-up tests
length(unique(follow_up_clean$employed_now)) == 2
length(unique(follow_up_clean$college_deg)) == 2
length(unique(follow_up_clean$employed_wife)) == 2
wife_outside_interview <- follow_up_clean |> filter(interviewed_out_fl == 1) |> nrow()
wife_outside_apply <- follow_up_clean |> filter(applied_out_fl == 1) |> nrow()
wife_outside_interview <= wife_outside_apply

# Online survey tests
c_number1 <- online_survey_clean |> filter(condition == 0) |> nrow()
c_number2 <- online_survey_clean |> filter(!is.na(c_list)) |> nrow()
c_number3 <- online_survey_clean |> filter(!is.na(c_outside_self)) |> nrow()
c_number4 <- online_survey_clean |> filter(!is.na(c_outside_wedge)) |> nrow()

t_number1 <- online_survey_clean |> filter(condition == 1) |> nrow()
t_number2 <- online_survey_clean |> filter(!is.na(t_outside_confidence)) |> nrow()
t_number3 <- online_survey_clean |> filter(!is.na(t_outside_guess_frac)) |> nrow()
t_number4 <- online_survey_clean |> filter(!is.na(t_outside_guess)) |> nrow()

c_number1 == c_number2
c_number3 == c_number4
c_number1 == c_number4
t_number1 == t_number2 
t_number3 == t_number4
t_number2 == t_number3

simulated_online_survey$age |> max() <= 35
simulated_online_survey$age |> min() >= 18
