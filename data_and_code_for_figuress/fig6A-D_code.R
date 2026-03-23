library(tidyverse)
library(patchwork)

# Colours
MidnightBlue <- "#00313C"
MiddayBlue <- "#00A9CE"

# Load data
fig6_individual_slopes_pct_time <- read_csv(
  "fig6_individual_slopes_pct_time.csv",
  show_col_types = FALSE
)
fig6_first_last_comparison <- read_csv(
  "fig6_first_last_comparison.csv",
  show_col_types = FALSE
)
fig6_individual_effects <- read_csv(
  "fig6_individual_effects.csv",
  show_col_types = FALSE
)

# Set plot theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    )
)

# Figure 6A
median_slope <- median(fig6_individual_slopes_pct_time$individual_slope)

plot_a <- ggplot(
  fig6_individual_slopes_pct_time,
  aes(x = individual_slope)
) +
  geom_histogram(bins = 30, fill = "grey70", alpha = 0.7, color = "black") +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = median_slope,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  labs(
    x = "Annual rate of change (β, log α-Gal sIgE / year)",
    y = "Number of people"
  )

# Figure 6B
median_pct <- median(fig6_individual_slopes_pct_time$annual_pct_change)

plot_b <- ggplot(
  fig6_individual_slopes_pct_time,
  aes(x = annual_pct_change)
) +
  geom_histogram(bins = 30, fill = "grey70", alpha = 0.7, color = "black") +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    color = "black",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = median_pct,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Annual percentage change in α-Gal sIgE",
    y = "Number of people"
  )

# Figure 6C
individual_effects <- fig6_individual_effects %>%
  mutate(Trajectory = ifelse(actual_slope < 0, "Decreasing", "Increasing"))

plot_c <- ggplot(
  individual_effects,
  aes(x = baseline_log_aGal, y = actual_slope)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
  geom_point(aes(color = Trajectory), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
  geom_vline(
    xintercept = log(0.1 + 1),
    linetype = "dotted",
    color = "black"
  ) +
  scale_color_manual(
    values = c("Increasing" = MidnightBlue, "Decreasing" = MiddayBlue),
    name = "Trajectory"
  ) +
  labs(
    x = "First test log(α-gal sIgE)",
    y = "Rate of decline (log units/year)"
  ) +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.box.background = element_rect(
      fill = "white",
      colour = "grey70",
      linewidth = 0.3
    )
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

# Figure 6D
first_last_comparison <- fig6_first_last_comparison %>%
  mutate(Change = ifelse(decreased, "Decreased", "Increased"))

plot_d <- ggplot(
  first_last_comparison,
  aes(x = first_log_aGal, y = last_log_aGal)
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
  geom_point(aes(color = Change), alpha = 0.6, size = 2) +
  scale_color_manual(
    values = c("Increased" = MidnightBlue, "Decreased" = MiddayBlue),
    name = "Change"
  ) +
  labs(x = "First test log(α-gal sIgE)", y = "Last test log(α-gal sIgE)") +
  theme(
    legend.position = c(0.99, 0.99),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.box.background = element_rect(
      fill = "white",
      colour = "grey50",
      linewidth = 0.3
    )
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  # Annotation lines for reference at log(0.1 +1)
  geom_hline(
    yintercept = log(0.1 + 1),
    linetype = "dotted",
    color = "grey20",
    lwd = 0.5
  ) +
  geom_vline(
    xintercept = log(0.1 + 1),
    linetype = "dotted",
    color = "grey20",
    lwd = 0.5
  )


# Combine plots
fig6_com_plot <- (plot_a + plot_b) /
  (plot_c + plot_d) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

fig6_com_plot

# Save plot
ggsave(
  "../../Manuscript/First fraft to all co-authors/Figures/Figure 6.png",
  fig6_com_plot,
  dpi = 300,
  bg = "white",
  height = 20,
  width = 24,
  units = "cm"
)
