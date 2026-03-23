library(tidyverse)
library(patchwork)

# Load data
sa3_cases_csv <- read_csv("sa3_cases_ESCat.csv", show_col_types = FALSE)

fig5_case_concentration_analysis <- sa3_cases_csv %>%
  filter(!is.na(c1MPPY)) %>%
  arrange(desc(c1MPPY)) %>%
  mutate(
    rank = row_number(),
    cumulative_cases = cumsum(c1MPPY),
    total_cases = sum(c1MPPY, na.rm = TRUE),
    cumulative_percentage = (cumulative_cases / total_cases) * 100,
    regions_percentage = (rank / n()) * 100
  )

# Set plot theme
theme_set(
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    )
)


# Figure 5A
plot_a <- ggplot(
  fig5_case_concentration_analysis,
  aes(x = regions_percentage, y = cumulative_percentage)
) +
  geom_line(color = "grey60", linewidth = 0.8) +
  geom_point(color = "grey10", size = 2, alpha = 0.5) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "red"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Percentage of SA3 regions (ranked by cases per 1M PPY)",
    y = "Cumulative % of total cases per 1M PPY"
  )


# Figure 5B
top_30 <- head(fig5_case_concentration_analysis, 30)
max_c1mppy <- max(top_30$c1MPPY, na.rm = TRUE)

plot_b <- ggplot(top_30, aes(x = reorder(SA_name, -c1MPPY))) +
  geom_col(aes(y = c1MPPY), fill = "grey60", alpha = 0.7) +
  geom_line(
    aes(y = cumulative_percentage * max_c1mppy / 100, group = 1),
    color = "grey10",
    linewidth = 0.6
  ) +
  geom_point(
    aes(y = cumulative_percentage * max_c1mppy / 100),
    color = "grey10",
    size = 1.5
  ) +
  scale_y_continuous(
    name = "Cases per 1M PPY",
    sec.axis = sec_axis(
      trans = ~ . * 100 / max_c1mppy,
      name = "Cumulative % of total cases"
    ),
    expand = c(0, 0)
  ) +
  labs(x = "SA3 regions ranked by cases per 1M PPY") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank()
  )

# Combine plots
fig5_com_plot <- plot_a /
  plot_b +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold"),
    plot.tag.position = c(-0.01, 1.01)
  )

fig5_com_plot

# Save plot
ggsave(
  "../../Manuscript/First fraft to all co-authors/Figures/Figure 5.png",
  fig5_com_plot,
  dpi = 300,
  bg = "white",
  height = 20,
  width = 24,
  units = "cm"
)
