# Read data file
raw_data <- read_excel("~/Desktop/data_science_projects/learning/reproductions/repro_graph_ai-scenarios/data/raw/0624data.xlsx", sheet = "Chart 1 Data")



ggplot(raw_data, aes(x = year)) +
  geom_line(aes(y = `real gdp per capita, 1990 $`, colour = "Real GDP per capita")) +
  geom_line(aes(y = ...10, colour = "Trend, 1870-2024"),linetype = "dotted") +
  geom_line(aes(y = ...9), linetype = "dashed", colour = "darkblue", linewidth = 1.5) +
  geom_line(aes(y = gdp_reverse), linetype = "dashed", colour = "darkred", linewidth = 1.5) +
  geom_line(aes(y = gdp_regline2), linetype = "dashed", colour = "darkgreen", linewidth = 1.5) +
  scale_colour_manual(
    name = element_blank(),
    breaks = c("Real GDP per capita", "Trend, 1870-2024"),
    values = c("Real GDP per capita" = "blue", "Trend, 1870-2024" = "purple")
  ) +
  scale_y_log10(
    labels = label_comma(),
    breaks = breaks_log(n = 14)
  ) +
  scale_x_continuous(
    breaks = seq(1870, 2050, 30)
  ) +
  labs(
    title = "AI could end scarcity, end humanity - or boost trend growth by 0.2 percentage points",
    subtitle = "US real GDP per capita (1990 $), log scale",
    x = "",
    y = ""
  ) +
  coord_cartesian(
    xlim = c(1870, 2050),
    ylim = c(1000, 500000),
    expand = FALSE
  ) +
  theme(
    plot.background = element_rect(fill = "#fff1e5"),
    panel.background = element_rect(fill = "#fff1e5"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "darkgrey", linewidth = 0.5),
    axis.ticks.length.x = unit(10, "pt"),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.ticks.y = element_blank(),
    plot.title = element_text(margin = margin(b = 15)),
    plot.subtitle = element_text(margin = margin(b = 15))
  ) +
  theme_sub_legend(
    position = "top",
    justification = "left",
    background = element_rect(fill = "#fff1e5")
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, linetype = "solid")))
