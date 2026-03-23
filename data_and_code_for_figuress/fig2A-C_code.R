library(tidyverse)
library(patchwork)

# Colours
LINE_COL <- "grey10"
TREND_COL <- "red"


# Load data
fig2_annual_data <- read_csv("fig2A-C_data.csv", show_col_types = FALSE)
fig2_fitted_tests <- read_csv("fig2_fitted_tests.csv", show_col_types = FALSE)
fig2_fitted_pos <- read_csv("fig2_fitted_pos.csv", show_col_types = FALSE)
fig2_fitted_PR <- read_csv("fig2_fitted_PR.csv", show_col_types = FALSE)
fig2_breakpoints <- read_csv("fig2_breakpoints.csv", show_col_types = FALSE)

# Set theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    )
)

# Figure 2A
plot_a <- ggplot(fig2_annual_data, aes(x = Year)) +
  geom_line(
    aes(y = total_tests),
    alpha = 0.8,
    fill = LINE_COL,
    linewidth = 0.6
  ) +
  geom_point(
    aes(y = total_tests),
    color = LINE_COL,
    size = 3,
    shape = 15
  ) +
  geom_ribbon(
    data = fig2_fitted_tests,
    aes(y = fitted, ymin = lower, ymax = upper),
    fill = TREND_COL,
    alpha = 0.15
  ) +
  geom_line(
    data = fig2_fitted_tests,
    aes(y = fitted),
    color = TREND_COL,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = fig2_breakpoints$breakpoint_tests,
    linetype = "twodash",
    color = "black",
    alpha = 0.5,
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(x = NULL, y = "Total number of α-gal sIgE tests")

# Figure 2B
plot_b <- ggplot(fig2_annual_data, aes(x = Year)) +
  geom_line(
    aes(y = positive_tests),
    color = LINE_COL,
    linewidth = 0.6
  ) +
  geom_point(
    aes(y = positive_tests),
    color = LINE_COL,
    size = 3,
    shape = 16
  ) +
  geom_ribbon(
    data = fig2_fitted_pos,
    aes(y = fitted, ymin = lower, ymax = upper),
    fill = TREND_COL,
    alpha = 0.15
  ) +
  geom_line(
    data = fig2_fitted_pos,
    aes(y = fitted),
    color = TREND_COL,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = fig2_breakpoints$breakpoint_pos,
    linetype = "twodash",
    color = "black",
    alpha = 0.5,
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(x = NULL, y = "Number of suspected MMA cases")

# Figure 2C
plot_c <- ggplot(fig2_annual_data, aes(x = Year)) +
  geom_line(aes(y = pos_prop), color = LINE_COL, linewidth = 0.6) +
  geom_point(aes(y = pos_prop), color = LINE_COL, size = 3, shape = 18) +
  geom_ribbon(
    data = fig2_fitted_PR,
    aes(y = fitted, ymin = lower, ymax = upper),
    fill = TREND_COL,
    alpha = 0.15
  ) +
  geom_line(
    data = fig2_fitted_PR,
    aes(y = fitted),
    color = TREND_COL,
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 2019,
    linetype = "twodash",
    color = "black",
    alpha = 0.5,
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, NA),
    labels = function(x) paste0(round(x, 1), "%")
  ) +
  labs(x = "Year", y = "Positivity rate (%)")

# Combine plots
fig2_com_plot <- plot_a /
  plot_b /
  plot_c +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.position = c(-0.01, 1.01)
  )

fig2_com_plot

# Save plot
ggsave(
  "../../Manuscript/First fraft to all co-authors/Figures/Figure 2.png",
  fig2_com_plot,
  dpi = 300,
  bg = "white",
  height = 20,
  width = 24,
  units = "cm"
)
