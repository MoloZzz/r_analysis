library(lattice)

# Штучні дані для contourplot
x <- seq(-3, 3, length = 50)
y <- seq(-3, 3, length = 50)
grid <- expand.grid(x = x, y = y)
grid$z <- with(grid, exp(-(x^2 + y^2)) * cos(x*2) * sin(y*2))

# Contourplot
contourplot(z ~ x * y, data = grid,
            region = TRUE,
            col.regions = terrain.colors(100),
            main = "Contourplot функції z = f(x, y)",
            xlab = "x", ylab = "y")
