
##############################################
# Аналіз даних про книги в бібліотеці
# Автор: Євчик Олексій
# Опис: генерація даних, обробка, візуалізація
##############################################

# Завантажуємо необхідні бібліотеки
library(dplyr)
library(ggplot2)

##############################################
# 1. Генерація даних
##############################################

set.seed(123)  # для відтворюваності результатів

# Створюємо вектор можливих жанрів
genres <- c("Фантастика", "Детектив", "Роман", "Наукова література", "Дитяча")

# Генеруємо таблицю з випадковими даними
books <- data.frame(
  Назва = paste("Книга", 1:100),
  Автор = paste("Автор", sample(1:50, 100, replace = TRUE)),
  Жанр = sample(genres, 100, replace = TRUE),
  Рік_видання = sample(1950:2024, 100, replace = TRUE),
  Кількість_сторінок = sample(80:800, 100, replace = TRUE)
)

# Переглядаємо перші рядки
head(books)

##############################################
# 2. Первинна обробка та очищення
##############################################

# Перевіримо на наявність пропусків
sum(is.na(books))  # має бути 0

# Приклад очищення — фільтруємо тільки книги після 1970 року
books_clean <- books %>% filter(Рік_видання > 1970)

##############################################
# 3. Візуалізації
##############################################

# 3.1. Кругова діаграма розподілу книг по жанрах
genre_count <- books_clean %>%
  count(Жанр)

pie_chart <- ggplot(genre_count, aes(x = "", y = n, fill = Жанр)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Розподіл книг за жанрами",
    fill = "Жанр"
  ) +
  theme_void()

# 3.2. Коробкова діаграма (boxplot) кількості сторінок по жанрах
box_plot <- ggplot(books_clean, aes(x = Жанр, y = Кількість_сторінок, fill = Жанр)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Розподіл кількості сторінок за жанрами",
    x = "Жанр",
    y = "Кількість сторінок"
  ) +
  theme_minimal()

# 3.3. Додаткова візуалізація — гістограма років видання
hist_plot <- ggplot(books_clean, aes(x = Рік_видання, fill = Жанр)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(
    title = "Розподіл книг за роком видання",
    x = "Рік видання",
    y = "Кількість книг"
  ) +
  theme_minimal()

##############################################
# 4. Збереження графіків у PNG та PDF
##############################################

# PNG
ggsave("pie_chart.png", plot = pie_chart, width = 6, height = 5, dpi = 300)
ggsave("box_plot.png", plot = box_plot, width = 6, height = 5, dpi = 300)
ggsave("hist_plot.png", plot = hist_plot, width = 6, height = 5, dpi = 300)

# --- PDF (через CairoPDF, щоб кирилиця не була крапками)
install.packages("Cairo")      # встановити 1 раз
library(Cairo)

CairoPDF("pie_chart.pdf", width = 6, height = 5)
print(pie_chart)
dev.off()

CairoPDF("box_plot.pdf", width = 6, height = 5)
print(box_plot)
dev.off()

CairoPDF("hist_plot.pdf", width = 6, height = 5)
print(hist_plot)
dev.off()

