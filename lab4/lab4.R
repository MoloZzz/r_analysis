library(ggplot2)
library(dplyr)

# 1) Підготовка даних
df <- mtcars %>%
  count(cyl, gear) %>%        # кількість авто за cyl × gear
  group_by(cyl) %>%
  mutate(total = sum(n))      # total за cyl

# 2) reorder(cyl, total)
df$cyl <- reorder(factor(df$cyl), df$total)

# 3) Побудова згрупованих стовпчиків
ggplot(df, aes(x = cyl, y = n, fill = factor(gear))) +
  geom_col(position = position_dodge()) +
  labs(x = "Cyl (упорядковано)", fill = "Gear") +
  theme_minimal()
