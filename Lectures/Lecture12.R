###### lecture 12 ###########################################################
library(exact2x2) # mcnemar.exact(rez)
library(pwr) # pwr.chisq.test()


(d.exm<-read.table("clipboard", head=T))
(rez <- table( d.exm[,c("pre","post")] ))
mcnemar.test(rez) # критерий Мак-Немара // по умолчанию с поправкой Эдвардса
mcnemar.test(rez, correct = FALSE)
mcnemar.exact(rez) # точный

### K таблиц сопряжённости 2х2 ##################################
drug <-
  array(c(11, 10, 25, 27,
          16, 22, 4, 10,
          14, 7, 5, 12,
          2, 1, 14, 16,
          6, 0, 11, 12,
          1, 0, 10, 10,
          1, 1, 4, 8,
          4, 6, 2, 1),
        dim = c(2, 2, 8),
        dimnames = list(
          Group = c("Drug", "Control"),
          Response = c("Success", "Failure"),
          Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug
mantelhaen.test(drug)

##### оценка мощности при сравнении долей ######################
# Согласно результатам опросов оказалось, что популярность кандидата
# Иванова у городских жителей выше, чем у жителей села (28% против
# 20% соответственно). Всего опрошено: по 100 респондентов в селе и городе

votes <- matrix(c(30, 70, 20, 80), ncol = 2, byrow = T)
votes
chisq.test(votes)
(res <- chisq.test(votes))
(obs <- res$observed)
(exptd <- res$expected)
(obs <- obs/200) # частоты
(exptd <- exptd/200)  # частоты
(sqrt(sum((exptd-obs)^2/exptd)) )# w - размер эффекта
ES.w2(obs) # функция для расчёта размера эффекта
pwr.chisq.test(w = ES.w2(obs), df = 1, N = 200)

pwr.chisq.test(w = 0.1, N = NULL, df = 1,
               sig.level = 0.05, power = 0.8)

##### проверка исходных предположений дисперсионного анализа ###
data(InsectSprays)
attach(InsectSprays)
plot(spray,count)
library(car)
leveneTest(count, spray)
tapply(count, spray, shapiro.test)
log(count + 1) # нормализация и стабилизация дисперсии
leveneTest(log(count + 1), spray)
tapply(log(count + 1), spray, shapiro.test)
