# штучні дані
values <- c(5, 18, 22, 35, 80, 90, 12, 50, 95)

# визначаємо кольори
cols <- ifelse(values < 20 | values > 80, "red", "gray70")
png("figure_lab2.png", width = 800, height = 600, res = 120)

# будуємо стовпчикову діаграму
barplot(values,
        col = cols,
        main = "Виділення крайніх значень",
        xlab = "Категорії",
        ylab = "Значення (%)")
dev.off()