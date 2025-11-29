library(ggplot2)
library(dplyr)
install.packages("tidyr")
library(tidyr)
library(scales)

set.seed(123)

data <- tibble(
  Group = rep(c("A", "B", "C"), each = 6),
  Category = rep(c("X", "Y"), times = 9),
  Value = round(runif(18, 10, 50), 1)
)

data

agg <- data %>%
  group_by(Group, Category) %>%
  summarise(
    mean_value = mean(Value),
    se = sd(Value) / sqrt(n()),
    .groups = "drop"
  )

agg

ggplot(agg, aes(x = Group, y = mean_value, fill = Category)) +
  geom_col(position = "dodge") +
  labs(
    title = "Проста стовпчикова діаграма",
    x = "Група",
    y = "Середнє значення"
  )

ggplot(agg, aes(x = Group, y = mean_value, fill = Category)) +
  geom_col(position = position_dodge()) +
  labs(title = "Згрупована діаграма")

ggplot(agg, aes(x = Group, y = mean_value, fill = Category)) +
  geom_col(position = "stack") +
  labs(title = "Стекова діаграма")

ggplot(agg, aes(x = Group, y = mean_value, fill = Category)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = percent(mean_value / tapply(mean_value, Group, sum)[Group])),
    position = position_fill(vjust = 0.5)
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Нормована (100%) діаграма")

ggplot(agg, aes(x = Group, y = mean_value, fill = Category)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_value - se, ymax = mean_value + se),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(title = "Столбчики з похибками (SE)")

data2 <- data %>%
  mutate(Type = rep(c("T1", "T2"), each = 9))

agg2 <- data2 %>%
  group_by(Type, Group, Category) %>%
  summarise(mean_value = mean(Value), .groups = "drop")


ggplot(agg2, aes(x = Group, y = mean_value, fill = Category)) +
  geom_col(position = "dodge") +
  facet_wrap(~Type) +
  labs(title = "Фасетування за Type")

pie_data <- data %>%
  count(Group) %>%
  mutate(pct = n / sum(n))

ggplot(pie_data, aes(x = "", y = pct, fill = Group)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = percent(pct)), position = position_stack(vjust = 0.5)) +
  labs(title = "Кругова діаграма")

ggplot(pie_data, aes(x = 2, y = pct, fill = Group)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = percent(pct)), position = position_stack(vjust = 0.5)) +
  xlim(0.5, 2.5) +
  labs(title = "Donut-діаграма") +
  theme_void()

