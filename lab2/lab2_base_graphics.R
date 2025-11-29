# Прикладні дані (реакція на два препарати у п'яти дозах)
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
df <- data.frame(Dose = dose, DrugA = drugA, DrugB =
                   drugB)

# Базовий графік для Drug A: точки + лінія
plot(dose, drugA, type = "b") # "b" = points + lines

opar <- par(no.readonly = TRUE) # зберегти поточні
налаштування
par(lty = 2, pch = 17, lwd = 2, cex = 1.3) 
plot(dose, drugA, type = "b", col = "red")
par(opar) # відновити параметри

n <- 10
pie(rep(1, n), col = rainbow(n), main =
      "rainbow()")
pie(rep(1, n), col = gray(0:n/n), main = "gray()")
pie(rep(1, n), col = heat.colors(n), main =
      "heat.colors()")

opar <- par(no.readonly = TRUE)
par(cex.main = 1.1, cex.lab = 0.95, cex.axis = 0.9, mar =
      c(5,4,4,2) + 0.1)
plot(dose, drugA, type = "b", col = "red",
     main = "Results of drug A", xlab = "Dose", ylab =
       "Effect")
par(opar)

x <- 1:10; y <- x; z <- 10/x
opar <- par(no.readonly = TRUE)
par(mar = c(5,4,4,8) + 0.1)
plot(x, y, type = "b", pch = 21, col = "red", yaxt = "n",
     lty = 3, ann = FALSE)
lines(x, z, type = "b", pch = 22, col = "blue", lty = 2)
axis(2, at = x, labels = x, col.axis = "red", las = 2)
axis(4, at = z, labels = round(z, 2), col.axis = "blue",
     las = 2, cex.axis = 0.8, tck = -0.01)
mtext("y = 1/x", side = 4, line = 3, col = "blue")
title("New charts", xlab = "X variable", ylab = "Y = X")
par(opar)

opar <- par(no.readonly = TRUE)
par(lwd = 2, cex = 1.2, font.lab = 2)
plot(dose, drugA, type = "b", pch = 15, lty = 1, col =
       "red", ylim = c(0, 60),
     main = "Comparison of drugs A and B", xlab = "Dose",
     ylab = "Effect")
lines(dose, drugB, type = "b", pch = 17, lty = 2, col =
        "blue")
abline(v = seq(20, 60, 10), lty = 2) # вертикальні
орієнтири
abline(h = seq(0, 60, 10), lty = 2) # горизонтальні
орієнтири
if (!requireNamespace("Hmisc", quietly = TRUE))
  install.packages("Hmisc")
Hmisc::minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
legend("topleft", inset = 0.03, title = "Drug", legend =
         c("A", "B"),
       lty = c(1,2), pch = c(15,17), col =
         c("red","blue"), bty = "n")
par(opar)

plot(mtcars$wt, mtcars$mpg, pch = 18, col = "blue",
     main = "Fuel consumption vs weight",
     xlab = "Weight (1000 lbs)", ylab = "MPG")
text(mtcars$wt, mtcars$mpg, labels = rownames(mtcars),
     cex = 0.6, pos = 4, col = "red")

png("comparison_AB.png", width = 1200, height = 800, res
    = 150)
plot(dose, drugA, type = "b", pch = 15, col = "red", ylim
     = c(0,60),
     main = "Comparison of drugs A and B", xlab = "Dose",
     ylab = "Effect")
lines(dose, drugB, type = "b", pch = 17, col = "blue",
      lty = 2)
legend("topleft", legend = c("A","B"), pch = c(15,17),
       col = c("red","blue"), bty = "n")
dev.off()
