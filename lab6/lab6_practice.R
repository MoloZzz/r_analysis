############################################################
# Лабораторна робота №6
# Візуалізація розподілених даних (Histogram, KDE, Boxplot)
# Практична частина
############################################################

### 0. Підготовка середовища -----------------------------------------------

packages <- c("ggplot2", "dplyr", "tidyr", "readr", "scales", "ggpubr")

to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

lapply(packages, library, character.only = TRUE)

set.seed(123)


### 1. Дані ------------------------------------------------------------------

# 1.1 Вбудований набір Old Faithful
data("faithful")   # колонки: eruptions, waiting

# 1.2 Синтетичні дані з двома модами
x1 <- rnorm(400, mean = 0, sd = 1)
x2 <- rnorm(300, mean = 3, sd = 0.7)
df_mix <- data.frame(x = c(x1, x2))


### 2. Гістограми ------------------------------------------------------------

# 2.1 Count histogram
p_h1 <- ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(binwidth = 0.2, fill = "grey70", color = "grey30") +
  labs(
    title = "Гістограма (лічильник)",
    x = "Тривалість виверження (хв)",
    y = "Кількість"
  ) +
  theme_minimal(base_size = 12)

# 2.2 Density histogram
p_h2 <- ggplot(faithful, aes(x = eruptions, y = after_stat(density))) +
  geom_histogram(binwidth = 0.2, fill = "grey70", color = "grey30") +
  labs(
    title = "Гістограма (щільність)",
    x = "Тривалість (хв)",
    y = "Щільність"
  ) +
  theme_minimal(base_size = 12)

# 2.3 Вплив binwidth
p_bw_small <- ggplot(df_mix, aes(x)) +
  geom_histogram(binwidth = 0.15, fill = "steelblue", color = "white") +
  labs(title = "Малий binwidth (деталі + шум)", x = "x", y = "Count") +
  theme_minimal(base_size = 12)

p_bw_large <- ggplot(df_mix, aes(x)) +
  geom_histogram(binwidth = 0.6, fill = "steelblue", color = "white") +
  labs(title = "Великий binwidth (згладження + ризик втрати мод)", x = "x", y = "Count") +
  theme_minimal(base_size = 12)


### 3. KDE та накладання ------------------------------------------------------

# 3.1 KDE окремо
p_kde <- ggplot(df_mix, aes(x)) +
  geom_density(linewidth = 1) +
  labs(title = "KDE (оцінка щільності)", x = "x", y = "Щільність") +
  theme_minimal(base_size = 12)

# 3.2 Гістограма + KDE
p_hist_kde <- ggplot(df_mix, aes(x, y = after_stat(density))) +
  geom_histogram(binwidth = 0.3, fill = "grey80", color = "grey40") +
  geom_density(linewidth = 1) +
  labs(title = "Гістограма + KDE", x = "x", y = "Щільність") +
  theme_minimal(base_size = 12)

# 3.3 Вплив параметра adjust
p_kde_adj06 <- ggplot(df_mix, aes(x)) +
  geom_density(adjust = 0.6, linewidth = 1) +
  labs(title = "KDE: adjust = 0.6 (детальніше)", x = "x", y = "Щільність") +
  theme_minimal(base_size = 12)

p_kde_adj10 <- ggplot(df_mix, aes(x)) +
  geom_density(adjust = 1.0, linewidth = 1) +
  labs(title = "KDE: adjust = 1.0 (баланс)", x = "x", y = "Щільність") +
  theme_minimal(base_size = 12)

p_kde_adj16 <- ggplot(df_mix, aes(x)) +
  geom_density(adjust = 1.6, linewidth = 1) +
  labs(title = "KDE: adjust = 1.6 (гладко)", x = "x", y = "Щільність") +
  theme_minimal(base_size = 12)


### 4. Boxplot + виявлення викидів -------------------------------------------

# 4.1 Boxplot з iris
p_box <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(outlier.colour = "red", width = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Boxplot: Sepal.Length за видами",
    x = "Вид",
    y = "Sepal.Length"
  ) +
  theme_minimal(base_size = 12)

# 4.2 Викиди за IQR-правилом
x <- faithful$eruptions

Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQRv <- IQR(x)

lower <- Q1 - 1.5 * IQRv
upper <- Q3 + 1.5 * IQRv

out_idx <- which(x < lower | x > upper)
cat("Кількість потенційних викидів:", length(out_idx), "\n")


### 5. Компоновка фігур -------------------------------------------------------

ggpubr::ggarrange(p_h1, p_h2, p_bw_small, p_bw_large,
                  ncol = 2, nrow = 2)

ggpubr::ggarrange(p_kde, p_hist_kde, p_kde_adj06,
                  p_kde_adj10, p_kde_adj16,
                  ncol = 2, nrow = 3)


### 6. Збереження результатів -------------------------------------------------

ggsave("hist_density_overlay.png", p_hist_kde,
       width = 8, height = 5, dpi = 300)

ggsave("box_iris.png", p_box,
       width = 7, height = 5, dpi = 300)
