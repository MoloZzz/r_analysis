library(lattice)
library(tidyverse)

# Прикладові дані
df <- tibble(
  Group = c("A", "B", "C", "D", "E"),
  Feature = c(12, 7, 19, 5, 14)
)

# Dotplot
dotplot(Group ~ Feature, data = df,
        main = "Dotplot порівняння груп",
        xlab = "Значення ознаки",
        ylab = "Група",
        col = "blue",
        pch = 19)
