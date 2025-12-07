library(ggplot2)

ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
