arr <- c(1.2, 1.4, 1.6, 1.7,1.7, 1.8, 2.2, 2.3, 2.4, 6.4, 19.0, 23.6)
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

hist(arr, breaks = 50, freq = TRUE, col = "lightblue",
      xlab = "Переменная X",
      ylab = "Плотность вероятности",
      main = "Гистограмма, совмещенная с кривой плотности")
lines(density(arr), col = "red", lwd = 2)

# Можно ли считать, что выборка извлечена из совокупности с нормальным распределением?
# Обоснуйте свой ответ.
#
# (Приведены результаты оценки проницаемости сосудов сетчатки)