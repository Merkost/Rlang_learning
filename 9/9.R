library(car)

df <- read.table('8/data.rd', encoding = "UTF-8")
healthy <- df[grepl('c', df$Nn),]
ill <- df[!grepl('c', df$Nn),]

median(healthy$C.4 - ill$C.4)

wilcox.test(healthy$C.4, head(ill$C.4, 60), paired = TRUE)
wilcox.test(healthy$C.6, head(ill$C.6, 60), paired = TRUE, conf.int = TRUE)

#графические тесты
hist(ill$C.4)
densityPlot(healthy$C.4)
qqnorm(ill$C.6)
qqPlot(healthy$C.6, distribution = "norm")
library(vioplot)
vioplot(ill$C.4, healthy$C.4, col = "gold", names=c('ill', 'healthy'))
#формальные тесты
ks.test(ill$C.6, "pnorm")
#Значение p-value < 2.2e-16, т.е. меньше порога 0.05,
# следовательно необходимо отвергнуть нулевую гипотезу и принять альтернативную.
# Распределение отличается от нормального.
shapiro.test(ill$C.6)
#Значение p-value меньше порога 0.05,
#следовательно нет оснований отвергнуть нулевую гипотезу.
#Нет оснований считать распределение отличным от нормального.

#t.test(head(ill$C.6, 60), healthy$C.6)


# Критерий Х2 Пирсона (Проверка гипотезы о нормальном распределении генеральной совокупности ).
chisq.test(healthy$C.4, head(ill$C.4, 60))
chisq.test(healthy$C.6, head(ill$C.6, 60))
# Критерий Фишера (Сравнение дисперсий двух нормальных генеральных совокупностей).
var.test(healthy$C.4, head(ill$C.4, 60))
var.test(healthy$C.6, head(ill$C.6, 60))
# Критерий Стьюдента (Сравнение двух средних нормальных генеральных совокупностей, дисперсии которых неизвестны и одинаковы).
t.test(healthy$C.4, head(ill$C.4, 60))
t.test(healthy$C.6, head(ill$C.6, 60))
# Критерии Бартлетта и Кохрана (Сравнение нескольких дисперсий нормальных генеральных совокупностей по выборкам).
bartlett.test(list(healthy$C.4, head(ill$C.4, 60)))
bartlett.test(list(healthy$C.6, head(ill$C.6, 60)))


power.t.test(delta = 1, sd = sd(healthy$C.4), sig.level = 0.05, power = 0.8)
power.t.test(delta = 1, sd = sd(healthy$C.4), sig.level = 0.01, power = 0.8)
power.t.test(delta = 1, sd = sd(healthy$C.4), sig.level = 0.05, power = 0.95)

power.t.test(delta = 1, sd = sd(ill$C.4), sig.level = 0.05, power = 0.8)
power.t.test(delta = 1, sd = sd(ill$C.4), sig.level = 0.01, power = 0.8)
power.t.test(delta = 1, sd = sd(ill$C.4), sig.level = 0.05, power = 0.95)


