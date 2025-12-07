library(lattice)
library(tidyverse)

# Дані для boxplot
set.seed(42)
df_box <- tibble(
  Group = rep(c("G1", "G2", "G3"), each = 50),
  Value = c(
    rnorm(50, 10, 3),
    rnorm(50, 15, 4),
    rnorm(50, 20, 5)
  )
)

# BWplot
bwplot(Value ~ Group, data = df_box,
       main = "Boxplot розподілу за групами",
       xlab = "Група", ylab = "Значення",
       col = "darkblue")
