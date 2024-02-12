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
f2 <- ggplot(df, aes(x = .data[[paste0(q, "_wedge_share")]])) +
  geom_histogram(binwidth = 10.5, fill = "lightgrey", color = "black") + # Adjust fill for contrast
  scale_x_continuous("Wedge (guess % - objective %)", 
                     breaks = seq(-100, 100, by = 10), 
                     limits = c(-100, 100), # Explicitly set x-axis limits
                     expand = c(0, 0.5)) +
  scale_y_continuous("Density", expand = c(0.5, 0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#FF4136", size = 0.5) + # Less prominent zero line
  labs(title = "Distribution of Wedge (Guess % - Objective %)", # Add a title
       caption = "Red dashed line represents the zero point") + # Add a caption
  theme_minimal(base_size = 12) + # Use theme_minimal with a base font size
  theme(
    plot.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"), # White background with border
    axis.text = element_text(size = 12, color = "black"), # Ensure text is black for readability
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_line(color = "lightgrey", size = 0.2), # Subtle gridlines
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5), # Center the title
    plot.caption = element_text(hjust = 0.5) # Center the caption
  )
f2


# Save the cleaned data
figure_path <- "/cloud/project/outputs/figures/figure_2.png"

ggsave(filename = figure_path, plot = f2, device = "png", width = 10, height = 6, dpi = 300)