arr <- c(289, 203, 359, 243, 232, 210, 251, 246, 224, 239, 220, 211)
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

hist(arr, breaks = 50, freq = TRUE, col = "lightblue",
      xlab = "Переменная X",
      ylab = "Плотность вероятности",
      main = "Гистограмма, совмещенная с кривой плотности")
lines(density(arr), col = "red", lwd = 2)
# Можно ли считать, что выборка извлечена из совокупности с нормальным распределением?
# Обоснуйте свой ответ.
#
# (Эти числа — продолжительность (в секундах) физической нагрузки до развития
# приступа стенокардии у 12 человек с ишемической болезнью сердца.