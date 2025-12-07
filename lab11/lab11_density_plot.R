library(lattice)

# Використовуємо вбудований набір даних iris
data("iris")

# Density plot з overlay
densityplot(~ Sepal.Length,
            group = Species,
            data = iris,
            auto.key = TRUE,
            plot.points = FALSE,
            main = "Порівняння розподілів Sepal.Length",
            xlab = "Довжина чашолистка")
