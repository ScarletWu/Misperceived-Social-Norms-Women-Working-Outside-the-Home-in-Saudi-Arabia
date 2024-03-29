library(dplyr)
library(ggplot2)
library(haven)

# Define path 
datadir <- "/cloud/project/inputs/data/"
data_path <- file.path(datadir, "clean_data/02_follow_up_clean.dta")

# Load the dataset
df <- read_dta(data_path)

# Keep matched responses and rename variable if necessary
df_filtered <- df %>%
  filter(matched == 1)

# Assuming 'driving_out_fl' is a binary outcome column already present in 'df'
# If not, the renaming step should be adjusted or clarified based on actual data structure

# Calculate rates and their confidence intervals
df_stats <- df_filtered %>%
  mutate(condition = if_else(is.na(condition), 0, condition)) %>% # Replace NA in condition before grouping
  group_by(condition) %>%
  summarise(
    n = n(),  # Total number of observations
    applied_rate = mean(applied_out_fl, na.rm = TRUE) * 100,
    interviewed_rate = mean(interviewed_out_fl, na.rm = TRUE) * 100,
    employed_now_rate = mean(employed_now_out_fl, na.rm = TRUE) * 100,
    driving_rate = mean(driving_fl, na.rm = TRUE) * 100  # Assuming driving_fl is the correct column name
  ) %>%
  mutate(
    applied_se = sqrt((applied_rate / 100) * (1 - (applied_rate / 100)) / n) * 100,
    applied_rate_lo = applied_rate - 1.96 * applied_se,
    applied_rate_hi = applied_rate + 1.96 * applied_se,
    interviewed_se = sqrt((interviewed_rate / 100) * (1 - (interviewed_rate / 100)) / n) * 100,
    interviewed_rate_lo = interviewed_rate - 1.96 * interviewed_se,
    interviewed_rate_hi = interviewed_rate + 1.96 * interviewed_se,
    employed_now_se = sqrt((employed_now_rate / 100) * (1 - (employed_now_rate / 100)) / n) * 100,
    employed_now_rate_lo = employed_now_rate - 1.96 * employed_now_se,
    employed_now_rate_hi = employed_now_rate + 1.96 * employed_now_se,
    driving_se = sqrt((driving_rate / 100) * (1 - (driving_rate / 100)) / n) * 100,  # SE for driving rate
    driving_rate_lo = driving_rate - 1.96 * driving_se,  # Lower CI for driving rate
    driving_rate_hi = driving_rate + 1.96 * driving_se   # Upper CI for driving rate
  ) %>%
  select(-n) # Optionally dropping 'n' after calculations


# p values
p_value_applied <- t.test(applied_out_fl ~ factor(condition), data = df_filtered, na.rm = TRUE)$p.value
p_value_interviewed <- t.test(interviewed_out_fl ~ factor(condition), data = df_filtered, na.rm = TRUE)$p.value
p_value_employed_now <- t.test(employed_now_out_fl ~ factor(condition), data = df_filtered, na.rm = TRUE)$p.value
p_value_driving <- t.test(driving_fl ~ factor(condition), data = df_filtered, na.rm = TRUE)$p.value


applied_rate_4a <- ggplot(df_stats, aes(x = factor(condition), y = applied_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = applied_rate_lo, ymax = applied_rate_hi), width = 0.2, color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", applied_rate), y = applied_rate + 1), position = position_dodge(width = 0.7), vjust = 0) +
  annotate("text", x = 1, y = max(df_stats$applied_rate_hi) + 3, label = paste("p-value =", round(p_value_applied, 3)), size = 4) +
  labs(x = "", y = "% Applied", title = "Long-Term Labor Supply Outcomes") +
  theme_minimal() +
  scale_x_discrete(labels = c("Control", "Treatment"))



ggsave("/cloud/project/outputs/figures/applied_rate_4a.png", plot = applied_rate_4a, width = 8, height = 6)

# Interviewed Rate

interviewed_rate_4b <- ggplot(df_stats, aes(x = factor(condition), y = interviewed_rate)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = interviewed_rate_lo, ymax = interviewed_rate_hi), width = 0.2, color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", interviewed_rate), y = interviewed_rate_hi + 2), vjust = 0) +
  annotate("text", x = 1.5, y = max(df_stats$interviewed_rate_hi) + 5, label = paste("p-value =", round(p_value_interviewed, 3)), size = 4) +
  labs(y = "% Interviewed", x = "") +
  theme_minimal() +
  scale_x_discrete(labels = c("Control", "Treatment"))

ggsave("/cloud/project/outputs/figures/interviewed_rate_4b.png", plot = interviewed_rate_4b, width = 8, height = 6)

# Employed Now Rate

employed_rate_4c <- ggplot(df_stats, aes(x = factor(condition), y = employed_now_rate)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = employed_now_rate_lo, ymax = employed_now_rate_hi), width = 0.2, color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", employed_now_rate), y = employed_now_rate_hi + 2), vjust = 0) +
  annotate("text", x = 1.5, y = max(df_stats$employed_now_rate_hi) + 5, label = paste("p-value =", round(p_value_employed_now, 3)), size = 4) +
  labs(y = "% Employed Now", x = "") +
  theme_minimal() +
  scale_x_discrete(labels = c("Control", "Treatment"))

ggsave("/cloud/project/outputs/figures/employed_rate_4c.png", plot = employed_rate_4c, width = 8, height = 6)




# Driving Rate
driving_rate_4d <- ggplot(df_stats, aes(x = factor(condition), y = driving_rate)) +
  geom_bar(stat = "identity", fill = "lightpink", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = driving_rate_lo, ymax = driving_rate_hi), width = 0.2, color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", driving_rate), y = driving_rate_hi + 2), vjust = 0) +
  annotate("text", x = 1.5, y = max(df_stats$driving_rate_hi) + 5, label = paste("p-value =", round(p_value_driving, 3)), size = 4) +
  labs(y = "% Driving", x = "Condition") +
  theme_minimal() +
  scale_x_discrete(labels = c("Control", "Treatment"))


ggsave("/cloud/project/outputs/figures/driving_rate_4d.png", plot = driving_rate_4d, width = 8, height = 6)


