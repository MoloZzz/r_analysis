x <- mtcars$hp
y <- mtcars$mpg

png("fig_lab2_task6.png", width = 800, height = 600, res = 120)

plot(x, y,
     main = "Графік: MPG vs HP",
     xlab = "Horsepower",
     ylab = "Miles per gallon",
     cex.main = 1.5,     # більший заголовок
     cex.lab  = 1.2,     # середні підписи осей
     cex.axis = 0.9,     # дрібніші мітки осей
     font = 2            # жирний стиль тексту
)

mtext("Дані з mtcars", side = 1, line = 4, cex = 0.9, font = 3)  # курсив
dev.off()