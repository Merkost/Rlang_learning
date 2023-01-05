###### Lecture 13 ###############################################
####### Дисперсионный анализ по Краскелу-Уоллису ###############

#каждый участник эксперимента (M - мужчины (13 чел.), F0 - небеременные женщины (9 чел.),
# F1 - беременные женщины (9 чел.)) принимал 250 мг кофеина, что соответствует примерно
#3 чашкам кофе, после чего дважды определяли концентрацию кофеина в крови и
#рассчитывали T_1/2(период полувыведения). Результаты представлены в таблице:
# (d.cofein <- read.table(pipe("pbpaste"), header=TRUE))
(d.cofein <- read.table("14/coffein.rd", header=TRUE))
summary(d.cofein)
(N<-13+9*2)
(R<-(N+1)/2) # Общий средний ранг
attach(d.cofein)
(R1<-sum(RM)/13) # средний ранг по группе M
(R2<-sum(RF0, na.rm = T)/9) # средний ранг по группе F0
(R3<-sum(RF1, na.rm = T)/9) # средний ранг по группе F1
(D<-13*(R1-R)^2+9*(R2-R)^2+9*(R3-R)^2)
(H<-D*12/(N*(N+1)))
qchisq(p = 0.95, df = 2) # табличное значение критерия хи-квадрат
detach(d.cofein)

coffein<-c(d.cofein$M,d.cofein$F0[1:9],d.cofein$F1[1:9])
category <-c(rep("M",13),rep("F0",9),rep("F1",9))
cat.f<-as.factor(category)
plot(cat.f,coffein)
kruskal.test(coffein ~ cat.f)

#### Множественные попарные сравнения ########################
(p3<-wilcox.test(coffein[1:22] ~ cat.f[1:22], paired = FALSE)$p.value )# M vs F0
(p2<-wilcox.test(coffein[14:31] ~ cat.f[14:31], paired = FALSE)$p.value  )# F1 vs F0
(p1<-wilcox.test(coffein[c(1:13,23:31)] ~ cat.f[c(1:13,23:31)], paired = FALSE)$p.value )# M vs F1

p.adjust(c(p1,p2,p3), method = "bonferroni")
p.adjust(c(p1,p2,p3), method = "holm")

#Пример: содержание стронция (мг/мл) в пяти водоемах США
waterbodies <- data.frame(Water = rep(c("Grayson", "Beaver",
                                        "Angler", "Appletree",
                                        "Rock"), each = 6),
                          Sr = c(28.2, 33.2, 36.4, 34.6, 29.1, 31.0,
                                 39.6, 40.8, 37.9, 37.1, 43.6, 42.4,
                                 46.3, 42.1, 43.5, 48.8, 43.7, 40.1,
                                 41.0, 44.1, 46.4, 40.2, 38.6, 36.3,
                                 56.3, 54.1, 59.4, 62.7, 60.0, 57.3)
)
M <- aov(Sr ~ Water, data = waterbodies)
summary(M)
TukeyHSD(M) # критерий Тьюки
par(mar = c(4.5, 8, 4.5, 4.5))
plot(TukeyHSD(M), las = 1)

library(DescTools) # реализован тест Даннета: множественные сравнения с контрольной группой
data <- data.frame(technique = rep (c("control", "new1", "new2"), each = 10 ),
                   score = c(76, 77, 77, 81, 82, 82, 83, 84, 85, 89,
                             81, 82, 83, 83, 83, 84, 87, 90, 92, 93,
                             77, 78, 79, 88, 89, 90, 91, 95, 95, 98))

boxplot(score ~ technique,
        data = data,
        main = "Экзаменационные баллы в зависимости от методики",
        xlab = "Методика обучения",
        ylab = "Экзаменационные баллы",
        col = c("green","blue","violet"),
        border = "black")

#fit the one-way ANOVA model
model <- aov(score ~ technique, data = data)
summary(model)
# тест Даннета, чтобы определить, какая методика обучения
# дает средние экзаменационные баллы, отличные от
# контрольной группы.
DunnettTest(x=data$score, g=data$technique)

airquality # New York Air Quality Measurements
boxplot(Ozone  ~ Month, data = airquality, col=2:6)
DunnettTest(Ozone  ~ Month, data = airquality)
DunnettTest(Ozone ~ Month, data = airquality, control="8", conf.level=0.9)

