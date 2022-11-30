vec <- c(4, 5, 5.5, 5, 2.5, 5.5, 5, 3.5, 2.5, 2.5, 6, 3.5, 4, 3.5, 1.5, 1.5, 3, 4.5, 4, 3.5)
length(vec)

# Среднеквадратическое отклонение
sd(vec)

# Стандартное отклонение (Среднее абсолютное)
mad(vec)

# Среднее
var(vec)

hist(vec, breaks = 50, freq = TRUE, col = "lightblue",
      xlab = "X",
      ylab = "freq",
      main = "Histogram")
lines(density(vec), col = "red", lwd = 2)