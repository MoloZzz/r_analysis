library(lattice)
library(tidyverse)

# Дані
df_bar <- tibble(
  Category = c("A", "B", "C", "D", "E"),
  Value = c(17, 9, 23, 15, 11)
)

# Barchart
barchart(Value ~ Category, data = df_bar,
         col = "skyblue",
         main = "Порівняльна стовпчикова діаграма",
         xlab = "Категорія", ylab = "Значення")
