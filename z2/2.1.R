arr <- c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5, 6, 7, 9, 10, 11)
print(arr)

# среднее
average <- mean(arr)
print(sprintf("Average: %f", average))

# стандартное отклонение
std <- sd(arr)
print(sprintf("Standard deviation: %f", std))

# медиана
median <- median(arr)
print(sprintf("Median: %f", median))

# процентили
qtl <- quantile(arr, c(0.25, 0.75))
qtl


summary(arr)
shapiro.test(arr) # Если P < 0.05 - нормальное распределение отвергается

hist(arr, breaks = 50, freq = TRUE, col = "lightblue",
      xlab = "X",
      ylab = "freq",
      main = "Histogram")
lines(density(arr), col = "red", lwd = 2)

# Можно ли считать, что выборка извлечена из совокупности с нормальным распределением?
# Обоснуйте свой ответ.

# (Приведенные числа — клинические оценки тяжести серповидноклеточной анемии).