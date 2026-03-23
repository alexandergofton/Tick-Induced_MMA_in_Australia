library(tidyverse)
library(patchwork)

# Define colours)

MALE_COL <- "#0072B2"
FEMALE_COL <- "#D55E00"

# Figure 2 data
rr_sex_by_age <- read_csv(
  "figS1_data_rr_sex_by_age.csv",
  show_col_types = FALSE
)

rr_age_only <- read_csv(
  "figS1_data_rr_age_only.csv",
  show_col_types = FALSE
)

combined_rr <- read_csv(
  "figS1_data_rr_combined_results.csv",
  show_col_types = FALSE
)

# Set plot theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    )
)

# Age categories order
age_order <- c(
  "0-14",
  "15-24",
  "25-34",
  "35-44",
  "45-54",
  "55-64",
  "65-74",
  "75-84",
  "85+"
)


# Figure S1 A
rr_local <- rr_sex_by_age %>%
  mutate(
    Age_cat = factor(Age_cat, levels = age_order),
    significant = ifelse(P_value < 0.05, "Significant", "Not Significant")
  )

plot_a <- ggplot(rr_local, aes(x = RR, y = Age_cat)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  geom_point(aes(shape = significant), color = "grey20", size = 3) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI),
    color = "grey20",
    height = 0.3,
    linewidth = 0.6
  ) +
  scale_shape_manual(
    values = c("Significant" = 16, "Not Significant" = 1)
  ) +
  scale_y_discrete(limits = rev) +
  labs(x = "Risk Ratio", y = "Age Category") +
  theme_minimal() +
  theme(legend.position = "none")

# Figure S1 B
age_only_order <- c(
  "0-14",
  "15-24",
  "35-44",
  "45-54",
  "55-64",
  "65-74",
  "75-84",
  "85+"
)

rr_local <- rr_age_only %>%
  mutate(
    Age_cat = factor(Age_cat, levels = age_only_order),
    significant = ifelse(P_value < 0.05, "Significant", "Not Significant")
  )

plot_b <- ggplot(rr_local, aes(x = RR, y = Age_cat)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  geom_point(aes(shape = significant), color = "grey20", size = 3) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI),
    color = "grey20",
    height = 0.3,
    linewidth = 0.6
  ) +
  scale_shape_manual(
    values = c("Significant" = 16, "Not Significant" = 1)
  ) +
  scale_y_discrete(limits = rev) +
  labs(x = "Risk Ratio", y = "Age Category") +
  theme_minimal() +
  theme(legend.position = "none")

# Figure S1 C
plot_data <- combined_rr %>%
  mutate(
    # Create a combined label for better visualization
    group_label = paste0(Age_cat, " ", Sex),
    # Create significance indicator
    significant = ifelse(P_value < 0.05, "Significant", "Not Significant"),
    # Create proper ordering: first by age, then by sex within age
    Age_cat = factor(
      Age_cat,
      levels = c(
        "0-14",
        "15-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75-84",
        "85+"
      )
    ),
    Sex = factor(Sex, levels = c("M", "F")),
    # Create ordered factor for y-axis that respects age then sex ordering
    group_label = factor(
      group_label,
      levels = expand_grid(
        Age_cat = c(
          "0-14",
          "15-24",
          "25-34",
          "35-44",
          "45-54",
          "55-64",
          "65-74",
          "75-84",
          "85+"
        ),
        Sex = c("M", "F")
      ) %>%
        filter(!(Age_cat == "25-34" & Sex == "F")) %>% # Remove reference
        mutate(label = paste(Age_cat, Sex)) %>%
        pull(label)
    )
  )

plot_data <- plot_data %>%
  mutate(
    Sex_label = case_when(
      Sex == "M" ~ "Male",
      Sex == "F" ~ "Female"
    )
  )

plot_c <- plot_data %>%
  ggplot(aes(x = RR, y = group_label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(color = Sex_label, shape = significant), size = 4) +
  geom_errorbarh(
    aes(xmin = Lower_CI, xmax = Upper_CI, color = Sex_label),
    height = 0.3,
    linewidth = 0.8
  ) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = MALE_COL, "Female" = FEMALE_COL)
  ) +
  scale_shape_manual(
    name = "Statistical Significance",
    values = c("Significant" = 16, "Not Significant" = 1),
    guide = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    #title = "Risk Ratios for Age-Sex Combinations",
    #subtitle = "Reference: Male 25-34 years",
    x = "Risk Ratio (RR)",
    y = "Age-Sex Category"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(shape = 16),
      order = 1
    ),
    shape = guide_legend(
      override.aes = list(color = "black"),
      order = 2
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing = unit(0.5, "cm"),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey70")
  )

# Extract the plot and legends
plot_c.1 <- plot_c +
  theme(
    axis.title.y = element_text(hjust = 0.5, margin = margin(r = 8)),
    legend.position = c(0.98, 0.3),
    legend.justification = c("right", "top"),
    #legend.text = element_text(size = 12),
    legend.background = element_rect(
      fill = "white",
      color = "grey70",
      linewidth = 0.3
    ),
    legend.margin = margin(5, 10, 5, 5),
    legend.box = "vertical"
  ) +
  guides(
    # Sex legend inside plot
    color = guide_legend(
      override.aes = list(shape = 16),
      title = NULL,
      order = 1
    ),
    # Significance legend at bottom - we'll handle this separately
    shape = "none"
  )

# Add the significance legend manually at the bottom
library(gridExtra)
library(grid)

# Create the significance legend separately
sig_legend <- data.frame(
  x = c(1, 2),
  y = c(1, 1),
  significant = c("Significant", "Not Significant")
)

sig_legend_plot <- ggplot(sig_legend, aes(x = x, y = y, shape = significant)) +
  geom_point(size = 4, color = "black") +
  scale_shape_manual(
    name = "Statistical Significance",
    values = c("Significant" = 16, "Not Significant" = 1)
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = "white",
      color = "white",
      linewidth = 0.5
    )
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3)))

# Extract just the legend
sig_legend_grob <- cowplot::get_legend(sig_legend_plot)

# Combine plot with bottom legend
library(cowplot)

plot_c <- plot_grid(
  plot_c.1,
  sig_legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.08)
)

plot_c


# Combine plots
figS1_com_plot <- (plot_a + plot_b) /
  plot_c +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

figS1_com_plot

# Save figS1_com_plot
ggsave(
  "../../Manuscript/First fraft to all co-authors/Figures/Figure S1.png",
  figS1_com_plot,
  dpi = 300,
  bg = "white",
  height = 20,
  width = 24,
  units = "cm"
)
