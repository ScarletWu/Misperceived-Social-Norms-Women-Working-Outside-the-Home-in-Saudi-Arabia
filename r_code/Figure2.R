# Load necessary libraries
library(haven) 
library(ggplot2) 
library(dplyr)

# Define the path
datadir <- "/cloud/project/inputs/data/"
data_path <- file.path(datadir, "clean_data/01_main_exp_clean.dta")

# Load the dataset
df <- read_dta(data_path)

# Define the variable prefix
q <- "outside"

# Create new variables
df[[paste0(q, "_guess_share")]] <- df[[paste0(q, "_guess")]] / 30
df[[paste0(q, "_others_truth")]] <- ifelse(df$stag == 1, df[[paste0(q, "_objective")]] / 30, NA)
q_self_mean <- mean(df[[paste0(q, "_self")]], na.rm = TRUE)
df[[paste0(q, "_wedge_share")]] <- (df[[paste0(q, "_wedge")]] / 30) * 100

# Plotting a histogram of the wedge share

f2 <- df |> ggplot(aes(x = (outside_wedge/30) * 100))+
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "grey", 
                 color = "black", 
                 na.rm = TRUE, 
                 breaks = seq(-100, 30, by = 10)) +   
  geom_vline(xintercept = 0, color = "red4", linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(name= "Wedge (guess% - objective %)", 
                     breaks = c(-100,-80,-60,-40,-20,0,20,40,60,80,100), 
                     limits = c(-100,100)) +
  scale_y_continuous(name = "Density", 
                     breaks = c(0, 0.005, 0.01, 0.015),
                     limits = c(0, 0.015))

f2
# Save the cleaned data
figure_path <- "/cloud/project/outputs/figures/figure_2.png"

ggsave(filename = figure_path, plot = f2, device = "png", width = 10, height = 6, dpi = 300)