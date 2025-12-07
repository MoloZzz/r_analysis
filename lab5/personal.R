if (!requireNamespace("viridisLite", quietly = TRUE)) {
  install.packages("viridisLite")
}

library(viridisLite)

z <- volcano
x <- seq_len(nrow(z))
y <- seq_len(ncol(z))

# Параметри палітри
ncols <- 200                             # кількість відтінків у палітрі (чим більше — тим плавніший градієнт)
palette_cols <- viridis(n = ncols, option = "magma")  # "cividis" — very colorblind-friendly
# Альтернативи: option = "viridis", "magma", "plasma", "inferno" (усі з viridis сім'ї; "cividis" для дальтонізму)

# Межі значень для легенди
zlim <- range(z, na.rm = TRUE)
at_ticks <- pretty(zlim, n = 6)         # розумні відмітки на легенді

# Малюємо filled.contour з власною легендою (key.axes)
filled.contour(
  x = x, y = y, z = z,
  color.palette = function(n) palette_cols,  # подаємо нашу палітру
  nlevels = ncols,
  zlim = zlim,
  plot.title = title(main = "Filled contour — палітра cividis (colorblind-friendly)",
                     xlab = "x", ylab = "y"),
  plot.axes = {
    axis(1); axis(2)
    # додаткові осі/мітки можна додати тут
  },
  key.title = title(main = "value"),      # заголовок колірної шкали
  key.axes = {
    # намалювати осі з бажаними відмітками у легенді
    axis(4, at = at_ticks, labels = format(at_ticks, digits = 3), las = 1)
  }
)
