# Задание 15. Регрессионный анализ

library(car)
library(vioplot)

group <- read.table('15/group.rd', header = T)
size <- length(group)
x <- group$`Рост`
y <- group$`Вес`
plot(x ~ y)
lm(x ~ y)

# Спроектируйте и реализуйте метод наименьших квадратов. Функцию сохраните.
least_squared_method <- function(x, y) {
  # Сделаем, чтобы все графики поместились на одном экране
  par(mfrow=c(2,2))

  (b <- cov(x, y) / var(x))
  (a <- mean(y) - mean(x) * b)
  plot(x, y, xlab = "Рост", ylab = "Вес") #График по данным группы
  lines(x, a + b * x, col = "cyan") #Теоретическая кривая
  plot(summary(lm(y ~ x))$residuals) #График остатков регрессии

  # Проверка остатков на наличие выбросов
  boxplot(x ~ y, data=group, notch=T, col=c("yellow", "blue"), xlab="Вес", ylab = "Рост")

  # Проверка остатков на нормальность
  # графические тесты
  hist(x)
  densityPlot(x)
  qqnorm(y)
  qqPlot(y, distribution = "norm")
  vioplot(x, y, col = "gold", names=c('Рост', 'Вес'))
  # формальные тесты
  ks.test(x, "rnorm")
  #Значение p-value < 2.2e-16, т.е. меньше порога 0.05,
  #следовательно необходимо отвергнуть нулевую гипотезу и принять альтернативную.
  #Распределение отличается от нормального.
  shapiro.test(y)
  # W = 0.88966, p-value = 0.02651
  #Значение p-value меньше порога 0.05,
  #следовательно нужно отвергнуть нулевую гипотезу.
  #Нет оснований считать распределение нормальным.
}

least_squared_method(x,y)
