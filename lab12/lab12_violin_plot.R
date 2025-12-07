ggplot(iris, aes(Species, Petal.Length, fill = Species)) +
  geom_violin(alpha = 0.4) +
  geom_boxplot(width = 0.1, color = "black")
