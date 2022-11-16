install.packages("ISwR")
library(car) # д.и. qqPlot()
library(sm) # д.и. sm.density()
library(ISwR) # данные по расходу энергии

boot_np <- function(data , Nboot=5000) {
  boots <- numeric(Nboot) # пустой вектор для хранения результатов

  for (i in 1:Nboot) {
    boots[i] <- mean(sample(data,replace=T))
  }

  CI <- quantile(boots, prob=c(0.025,0.975))
  return (c(m = mean(data),CI))
}
param_CI <- function(data) {
  n <- length(data)
  m <- mean(data)
  SE <- sd(data)/sqrt(n)
  E <- qt(.975, df=n-1)*SE
  CI <- m + c(-E, E)
  return (c(m , CI))
}
set.seed(337)
y<-rnorm(10, mean=0, sd=1)

param_CI(y)
boot_np(y)

salary <- c(21, 19, 27, 11, 102, 25, 21)

qqnorm(y)
qqline(y,col=2)

qqPlot(y, dist= "norm", pch=18, xlab="Квантили нормального распределения",
       ylab="Наблюдаемые квантили", main="Уравнение квантилей Ё– и Ќ–")

sm.density(y, model = "Normal", xlab="»митированная выборка",
           ylab="Функция плотности распределения")
shapiro.test(y)
ks.test(y, "pnorm")


data(energy) # суточный расход энергии (expend) у худощавых женщин (lean) и женщин с избыточным весом (obese)
attach(energy)
head(energy)
tapply(expend, stature, mean)
t.test(expend ~ stature)#различаются ли эти средние значения статистически?
t.test(expend ~ stature, var.equal = TRUE) # без поправки ”элча

load("A.rd")  # скорость решения задач (сек.) до и после курса
A
A[,3] - A[,2] # »ндивидуальные разности
mean(A[,3] - A[,2])
t.test(A[,3], A[,2], paired = TRUE)
