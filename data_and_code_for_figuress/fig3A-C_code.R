library(tidyverse)
library(patchwork)

# Figure 3 data
fig3_decomposition <- read_csv("fig3_decomposition.csv", show_col_types = FALSE)
fig3_sa3_expansion_with_se <- read_csv(
  "fig3_sa3_expansion_with_se.csv",
  show_col_types = FALSE
)

# Set plot theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    )
)

# Figure 3A
geo_model <- lm(n_sa3_regions ~ YOT, data = fig3_sa3_expansion_with_se)
geo_predictions <- data.frame(
  YOT = fig3_sa3_expansion_with_se$YOT,
  fit = predict(geo_model, se.fit = TRUE)$fit,
  se = predict(geo_model, se.fit = TRUE)$se.fit
) %>%
  mutate(lower = fit - 1.96 * se, upper = fit + 1.96 * se)

plot_a <- ggplot(fig3_sa3_expansion_with_se, aes(x = YOT, y = n_sa3_regions)) +
  geom_ribbon(
    data = geo_predictions,
    aes(y = fit, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  geom_line(color = "grey10", linewidth = 0.8) +
  geom_point(color = "grey10", size = 3) +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Number of SA3 regions")

# Figure 3B
plot_b <- ggplot(
  fig3_sa3_expansion_with_se,
  aes(x = YOT, y = mean_tests_per_region)
) +
  geom_col(fill = "grey60", alpha = 0.7, width = 0.9) +
  geom_errorbar(
    aes(
      ymin = mean_tests_per_region - se_tests,
      ymax = mean_tests_per_region + se_tests
    ),
    width = 0.3,
    linewidth = 0.4,
    colour = "grey10"
  ) +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Mean tests per region (± SE)")

# Figure 3C
contrib_data <- fig3_decomposition %>%
  select(YOT, geographic_effect, intensity_effect) %>%
  pivot_longer(-YOT, names_to = "component", values_to = "percent")

plot_c <- ggplot(
  contrib_data,
  aes(x = YOT, y = percent, fill = component)
) +
  geom_col(position = "dodge", alpha = 0.7, width = 0.9) +
  scale_fill_manual(
    values = c(
      "geographic_effect" = "grey60",
      "intensity_effect" = "grey10"
    ),
    labels = c(
      "geographic_effect" = "Geographic Expansion",
      "intensity_effect" = "Testing Intensity"
    )
  ) +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(x = "Year", y = "Contribution to testing growth", fill = NULL) +
  theme(
    legend.position = c(0.05, 0.84),
    legend.justification = c("left", "top"),
    legend.text = element_text(size = 10),
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


# Combine plots
fig3_com_plot <- plot_a /
  plot_b /
  plot_c +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

fig3_com_plot

# Save plot
ggsave(
  "../../Manuscript/First fraft to all co-authors/Figures/Figure 3.png",
  fig3_com_plot,
  dpi = 300,
  bg = "white",
  height = 20,
  width = 24,
  units = "cm"
)
