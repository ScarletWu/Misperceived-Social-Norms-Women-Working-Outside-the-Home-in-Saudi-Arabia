# Load necessary libraries
library(haven) 
library(ggplot2) 
library(dplyr)

# Define the path to your dataset
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
f2 <- ggplot(df, aes(x = .data[[paste0(q, "_wedge_share")]])) +
  geom_histogram(binwidth = 10.5, fill = "#d3d3d3", color = "black") +
  scale_x_continuous("Wedge (guess % - objective %)", breaks = seq(-100, 100, by = 20)) +
  scale_y_continuous("Density", expand = c(0, 0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "#e5e5e5"),
    panel.grid.minor = element_blank()
  )


# Save the cleaned data
figure_path <- "/cloud/project/outputs/figures/figure_2.png"

ggsave(filename = figure_path, plot = f2, device = "png", width = 10, height = 6, dpi = 300)