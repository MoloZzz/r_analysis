library(tidyverse)
library(lubridate)

# Прикладні дані
set.seed(1214413)
df <- tibble(
  date = seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day"),
  value = abs(rnorm(length(date), mean = 50, sd = 20))
)

# Підготовка даних для heatmap
season_df <- df %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(year, month) %>%
  summarise(activity = mean(value), .groups = "drop")

# Теплокарта
ggplot(season_df, aes(x = month, y = factor(year), fill = activity)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Heatmap сезонності",
    x = "Місяць",
    y = "Рік",
    fill = "Активність"
  ) +
  theme_minimal()
