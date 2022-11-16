install.packages("ISwR")
library(car) # д.и. qqPlot()
library(sm) # д.и. sm.density()
library(ISwR) # данные по расходу энергии

boot_np <- function(data, Nboot=5000) {
  boots <- numeric(Nboot) # Пустой вектор для хранения результатов

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
  E <- qt(.975, df=n-1) * SE
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
       ylab="Наблюдаемые квантили", main="Сравнение квантилей ЭР и НР")

sm.density(y, model = "Normal", xlab="Имитированная выборка",
           ylab="Функция плотности распределения")
shapiro.test(y)
ks.test(y, "pnorm")


data(energy) # суточный расход энергии (expend) у худощавых женщин (lean) и женщин с избыточным весом (obese)
attach(energy)
head(energy)
tapply(expend, stature, mean)
t.test(expend ~ stature)#различаются ли эти средние значения статистически?
t.test(expend ~ stature, var.equal = TRUE) # без поправки Уэлча

load("A.rd")  # скорость решения задач (сек.) до и после курса
A
A[,3] - A[,2] # Индивидуальные разности
mean(A[,3] - A[,2])
t.test(A[,3], A[,2], paired = TRUE)



#November 10!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805,
              7515, 7515, 8230, 8770)#суточное потребление энергии
wilcox.test(d.intake, mu = 7725)#отличается ли от нормативного значения 7725 кДж/сутки:
# ручной расчёт статистики W #
d.intake-7725
sort(abs(d.intake-7725))
3+5 # сумма рангов положительных элементов в ранжированном ряду

data(energy) # из пакета ISwR
attach(energy)
str(energy)
wilcox.test(expend ~ stature, paired = FALSE)

load("A.rd")  # скорость решения задач (сек.) до и после курса
A
A[,3] - A[,2] # Индивидуальные разности
median(A[,3] - A[,2])
wilcox.test(A[,3], A[,2], paired = TRUE)
wilcox.test(A[,3], A[,2], paired = TRUE, conf.int = TRUE)

power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, power = 0.8)
power.t.test(delta = 3.0, sd = 1.8, sig.level = 0.05, power = 0.8,
             type = "paired")

var.test(expend ~ stature)

data(InsectSprays) # данные получены в ходе эксперимента по изучению
# эффективности шести видов инсектицидных средств. Каждым из этих средств
# обработали по 12 растений, после чего подсчитали количество выживших
# на растениях насекомых.
attach(InsectSprays)
plot(spray,count)
library(car)
leveneTest(count ~ spray, data = InsectSprays)
leveneTest(count ~ spray, data = InsectSprays, center = mean)
bartlett.test(count ~ spray, data = InsectSprays)
fligner.test(count ~ spray, data = InsectSprays)
