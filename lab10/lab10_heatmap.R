library(lattice)
library(tidyverse)
library(lubridate)

# Прикладові дані
set.seed(123)
df_hm <- tibble(
  date = seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "day"),
  value = abs(rnorm(length(date), mean = 50, sd = 20))
)

# Агрегування за місяцем та роком
hm_data <- df_hm %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(mean_value = mean(value), .groups = "drop")

# Heatmap
levelplot(mean_value ~ month * year, data = hm_data,
          col.regions = colorRampPalette(viridisLite::viridis(20)),
          xlab = "Місяць", ylab = "Рік",
          main = "Heatmap середніх значень")
