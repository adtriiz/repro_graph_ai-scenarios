# library(tidyverse)
# library(scales)
# library(readxl)

# Read data file
# I should be able to reproduce it only importing the year and real gdp per capita, 1990$ columns
raw_data <- select(read_excel("~/Desktop/data_science_projects/learning/reproductions/repro_graph_ai-scenarios/data/raw/0624data.xlsx", range = "Chart 1 Data!B1:D182"),!country)

# We obtain the Average rate of YoY growth (until 2020!)
clean_data <- raw_data |> 
  mutate(
    YoY_growth = (`real gdp per capita, 1990 $` - lag(`real gdp per capita, 1990 $`))/lag(`real gdp per capita, 1990 $`)
  )

avg_YoY_growth <- clean_data |> 
  filter(year < 2021) |> 
  pull(YoY_growth) |> 
  mean(na.rm = TRUE)

# And fit a trend line from the last year based on that rate of growth
clean_data <- clean_data |> 
  mutate(
    years_from_2024 = max(which(!is.na(clean_data$`real gdp per capita, 1990 $`))) - row_number(),
    fit_line = last(clean_data$`real gdp per capita, 1990 $`, na_rm = TRUE)/(1+avg_YoY_growth)^years_from_2024
  )

# Create 2.1% growth projection
ai_normal_growth <- 0.021
clean_data <- clean_data |> 
  mutate(
    AI_normal = if_else(year >= 2024, last(clean_data$`real gdp per capita, 1990 $`, na_rm = TRUE)/(1+ai_normal_growth)^years_from_2024,NA)
  )

# Create acceleration data
## See 03_reproduce_data_acceleration.R to see why I just take the data as is
sequence_acceleration <- c(38689, 43763, 50791, 60554, 74147, 93101, 119560, 156527, 
                           208205, 280478, 381585, 523058, 721044, 998149, 1386021, 
                           1928965, 2689010, 3752998, 5242505, 7327739, 10246989, 
                           14333866, 20055416, 28065508, 39279564, 54979164, 76958528)

## Index-based assignment
rows_2024_onward <- which(clean_data$year >= 2024)
clean_data$AI_acceleration <- NA
clean_data$AI_acceleration[rows_2024_onward] <- sequence_acceleration

# Create extinction data
sequence_extinction <- c(38689, 38102, 36716, 35243, 33678, 32014, 30243, 28360, 
                         26355, 24220, 21946, 19524, 16943, 14191, 11258, 8130, 
                         4793, 1233, -2565, -6619, -10946, -15565, -20497, -25764, 
                         -31389, -37397, -43816)

clean_data$AI_extinction <- NA
clean_data$AI_extinction[rows_2024_onward] <- sequence_extinction

ggplot(clean_data, aes(x = year)) +
  geom_line(aes(y = `real gdp per capita, 1990 $`, colour = "Real GDP per capita")) +
  geom_line(aes(y = fit_line, colour = "Trend, 1870-2024"),linetype = "dotted") +
  geom_line(aes(y = AI_normal), linetype = "dashed", colour = "darkblue", linewidth = 1) +
  geom_line(aes(y = AI_extinction), linetype = "dashed", colour = "darkred", linewidth = 1) +
  geom_line(aes(y = AI_acceleration), linetype = "dashed", colour = "darkgreen", linewidth = 1) +
  scale_colour_manual(
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
    background = element_rect(fill = "#fff1e5"),
    title = element_blank()
  ) +
  guides(colour = guide_legend(
    override.aes = list(linewidth = 2, linetype = "solid")
    )
  )
