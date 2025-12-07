# ============================================
# 1. Завантаження бібліотек
# ============================================
# install.packages(c("fmsb", "tidyverse", "ggplot2", "plotly", "reshape2", "pheatmap", "scales"))

library(fmsb)
library(tidyverse)
library(scales)
library(ggplot2)
library(reshape2)
library(plotly)
library(pheatmap)

# ============================================
# 2. Радіальна діаграма (fmsb)
# ============================================

df <- data.frame(
  Accuracy = c(85, 78),
  Recall = c(80, 82),
  Precision = c(88, 75),
  F1 = c(84, 78),
  AUC = c(90, 81),
  Kappa = c(72, 68)
)
rownames(df) <- c("Model_A", "Model_B")

df_scaled <- rbind(apply(df, 2, max), apply(df, 2, min), df)

radarchart(
  df_scaled,
  axistype = 1,
  pcol = c("#1b9e77", "#d95f02"),
  pfcol = scales::alpha(c("#1b9e77", "#d95f02"), 0.3),
  plwd = 2,
  title = "Радіальна діаграма: профілі метрик"
)

legend("topright", legend = rownames(df), col = c("#1b9e77", "#d95f02"), lwd = 2, bty = "n")

# ============================================
# 3. Радіальна діаграма 0..1 нормована
# ============================================

norm01 <- function(x) (x - min(x)) / (max(x) - min(x) + 1e-9)
df01 <- as.data.frame(lapply(df, norm01))
df01_scaled <- rbind(apply(df01, 2, max), apply(df01, 2, min), df01)

radarchart(df01_scaled, axistype = 1, title = "Радіальна (нормована 0..1)")

# ============================================
# 4. Рельєфні графіки (volcano)
# ============================================

z <- volcano
x <- 1:nrow(z)
y <- 1:ncol(z)

contour(x, y, z, main = "Ізолінії (volcano)", xlab = "X", ylab = "Y")

filled.contour(x, y, z, color.palette = terrain.colors,
               plot.title = title(main = "Заповнені контури (volcano)"))

# 3D поверхня (base R)
persp(x, y, z, theta = 135, phi = 25, col = "lightblue", shade = 0.5,
      ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z",
      main = "3D-поверхня (volcano)")

# plotly 3D поверхня
plot_ly(z = ~volcano) %>%
  add_surface() %>%
  layout(title = "Plotly Surface: volcano")

# ============================================
# 5. Рельєф у ggplot2 (filled contours)
# ============================================

df_volc <- melt(volcano)
colnames(df_volc) <- c("X", "Y", "Z")

ggplot(df_volc, aes(X, Y, z = Z)) +
  geom_contour_filled() +
  labs(title = "ggplot2: заповнені контури", fill = "Висота") +
  coord_fixed()

# ============================================
# 6. Heatmap кореляцій (ggplot2)
# ============================================

data(mtcars)
cor_m <- cor(mtcars)
mcor <- melt(cor_m)

ggplot(mcor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#313695", mid = "white", high = "#a50026", midpoint = 0) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Теплова карта кореляцій mtcars", x = "", y = "", fill = "r")

# ============================================
# 7. Heatmap з кластеризацією (pheatmap)
# ============================================

pheatmap(cor_m,
         color = colorRampPalette(c("#313695", "white", "#a50026"))(100),
         display_numbers = TRUE, number_format = "%.2f",
         main = "pheatmap: кореляції mtcars",
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "complete")

