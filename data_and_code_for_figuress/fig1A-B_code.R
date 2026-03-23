library(tidyverse)
library(patchwork)

# Define colours)

MALE_COL <- "#0072B2"
FEMALE_COL <- "#D55E00"

# Figure 1 data
fig1_all_tested <- read_csv("fig1A_data.csv", show_col_types = FALSE)
fig1_positive_cases <- read_csv("fig1B_data.csv", show_col_types = FALSE)

# Set theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(0.98, 0.98),
      legend.justification = c("right", "top"),
      legend.text = element_text(size = 8),
      legend.title = element_blank(),
      legend.key.width = unit(1, "cm"), # Add this line - width of rectangle
      legend.key.height = unit(0.4, "cm"), # Add this line - height of rectangle
      legend.box.background = element_rect(
        fill = "white",
        color = "grey70",
        linewidth = 0.3
      ),
      legend.margin = margin(2, 2, 2, 2)
    )
)

# Figure 1A
plot_a <- ggplot(
  fig1_all_tested,
  aes(x = Age_cat, y = count, fill = Sex)
) +
  geom_col(
    alpha = 0.7,
    position = position_dodge(width = 0.85),
    width = 0.8
  ) +
  labs(x = "Age Category", y = "Count", fill = "Sex") +
  scale_fill_manual(
    values = c("M" = MALE_COL, "F" = FEMALE_COL),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1800))

# Figure 1B
plot_b <- ggplot(
  fig1_positive_cases,
  aes(x = Age_cat, y = count, fill = Sex)
) +
  geom_col(
    alpha = 0.7,
    position = position_dodge(width = 0.85),
    width = 0.8
  ) +
  labs(x = "Age Category", y = "Count", fill = "Sex") +
  scale_fill_manual(
    values = c("M" = MALE_COL, "F" = FEMALE_COL),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  scale_y_continuous(expand = c(0, 0))

# Combine plots
fig1_com_plot <- plot_a /
  plot_b +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

fig1_com_plot

# Save fig1_com_plot
ggsave(
  "../../Manuscript/First fraft to all co-authors/Figures/Figure 1.png",
  fig1_com_plot,
  dpi = 300,
  bg = "white",
  height = 12,
  width = 18,
  units = "cm"
)
