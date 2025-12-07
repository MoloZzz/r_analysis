library(ggplot2)

data(iris)

ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Криві густини довжини чашолистка",
    x = "Sepal Length",
    y = "Density"
  ) +
  theme_minimal()
