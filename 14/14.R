# Задание 14. Дисперсионный анализ по Краскелу-Уоллису
library(DescTools)

# 1.	Используя данные по инсектицидным спреям (data(InsectSprays),
# определите, отличаются ли спреи по эффективности

# a.	Выполните Дисперсионный анализ по Краскелу-Уоллису вручную
# (рассчитайте значение критерия Краскала—Уоллиса по формуле);
data(InsectSprays)
sprays <- as.data.frame(InsectSprays)
summary(sprays)

sprays$spray <- as.factor(sprays$spray)

# Упорядочиваем значения для подсчета рангов
sprays <- sprays[order(sprays$count),]

# Рассчитываем ранги (воспользуемся сервисом https://math.semestr.ru/group/rang.php)
# Чтобы не расставлять ранги вручную
rangs <- c(1.5,1.5,5.5,5.5,5.5,5.5,5.5,5.5,10.5,10.5,10.5,10.5,16.5,16.5,16.5,16.5,16.5,16.5,16.5,16.5,22.5,22.5,22.5,22.5,28,28,28,28,28,28,28,33,33,33,36,36,36,38,40,40,40,43,43,43,45.5,45.5,48.5,48.5,48.5,48.5,52.5,52.5,52.5,52.5,55.5,55.5,57.5,57.5,60.5,60.5,60.5,60.5,63,64.5,64.5,66.5,66.5,68,69,70,71.5,71.5)
sprays["rangs"] <- rangs

# Упорядочиваем по спреям для дальнейшего удобства
sprays <- sprays[order(sprays$spray),]

(N<-length(unique(sprays$spray))*12)
# 72

#(R<-(N+1)/2) # Общий средний ранг, то же значение, что ниже
(R<-sum(sprays$rangs/N))  # Общий средний ранг
# 36.5

# Рассчитываем средние ранги по группам
(R1<-sum(sprays$rangs[sprays$spray == "A"])/12) # средний ранг по группе A
# 52.16667
(R2<-sum(sprays$rangs[sprays$spray == "B"])/12) # средний ранг по группе B
# 54.83333
(R3<-sum(sprays$rangs[sprays$spray == "C"])/12) # средний ранг по группе C
# 11.45833
(R4<-sum(sprays$rangs[sprays$spray == "D"])/12) # средний ранг по группе D
# 25.58333
(R5<-sum(sprays$rangs[sprays$spray == "E"])/12) # средний ранг по группе E
# 19.33333
(R6<-sum(sprays$rangs[sprays$spray == "F"])/12) # средний ранг по группе F
# 55.625

(D<-12*(R1-R)^2 + 12*(R2-R)^2 + 12*(R3-R)^2 + 12*(R4-R)^2 + 12*(R5-R)^2 + 12*(R6-R)^2)
# 23859.29
(H<-D*12/(N*(N+1)))
# 54.47327

# Чем больше значение Н-критерия, тем больше у оснований отклонить нулевую гипотезу
# об отсутствии разницы между сравниваемыми группами.
# Если рассчитанное по выборочным данным значение Н превышает определенное
# критическое значение, нулевая гипотеза отклоняется.

qchisq(p = 0.95, df = 6-1) # табличное значение критерия хи-квадрат
# 11.0705

# При сравнении H-критерия с критическими значениями
# критерия хи-квадрат для числа степеней свободы 6-1 видно,
# что значение Н превышает табличное значение критерия хи-квадрат,
# поэтому нулевая гипотеза отклоняется.

# b.	Воспользуйтесь стандартной функцией kruskal.test() и сравните результат
kruskal.test(sprays$count ~ as.factor(sprays$spray))
# Kruskal-Wallis chi-squared = 54.691, df = 5, p-value = 1.511e-10

# Сравниваемые группы статистически значимо различаются, если Р.value < 0.05
# Cтатистически значимых различий между группами нет, если Р.value > 0.05)

# 2.	Проведите попарные сравнения и найдите спреи, которые значимо отличаются по эффективности
# a.	Использовать поправку Бонферони
# b.	Использовать поправку Холма
# c.	Сравните результаты предыдущих двух тестов с тем, что даёт критерий Тьюки

group <- c(rep("A", 12), rep("B", 12), rep("C", 12), rep("D", 12), rep("E", 12), rep("F", 12))
group[c(1:12,25:36)]

group_A <- sprays[sprays$spray == "A",]
group_B <- sprays[sprays$spray == "B",]
group_C <- sprays[sprays$spray == "C",]
group_D <- sprays[sprays$spray == "D",]
group_E <- sprays[sprays$spray == "E",]
group_F <- sprays[sprays$spray == "F",]

group_AB <- rbind(group_A, group_B)
group_AC <- rbind(group_A, group_C)
group_AD <- rbind(group_A, group_D)
group_AE <- rbind(group_A, group_E)
group_AF <- rbind(group_A, group_F)

group_BC <- rbind(group_B, group_C)
group_BD <- rbind(group_B, group_D)
group_BE <- rbind(group_B, group_E)
group_BF <- rbind(group_B, group_F)

group_CD <- rbind(group_C, group_D)
group_CE <- rbind(group_C, group_E)
group_CF <- rbind(group_C, group_F)

group_DE <- rbind(group_D, group_E)
group_DF <- rbind(group_D, group_F)

group_EF <- rbind(group_E, group_F)

(g1 <- wilcox.test(group_AB$count  ~ group_AB$spray)$p.value)
(g2 <- wilcox.test(group_AC$count  ~ group_AC$spray)$p.value)
(g3 <- wilcox.test(group_AD$count  ~ group_AD$spray)$p.value)
(g4 <- wilcox.test(group_AE$count  ~ group_AE$spray)$p.value)
(g5 <- wilcox.test(group_AF$count  ~ group_AF$spray)$p.value)

(g6 <- wilcox.test(group_BC$count  ~ group_BC$spray)$p.value)
(g7 <- wilcox.test(group_BD$count  ~ group_BD$spray)$p.value)
(g8 <- wilcox.test(group_BE$count  ~ group_BE$spray)$p.value)
(g9 <- wilcox.test(group_BF$count  ~ group_BF$spray)$p.value)

(g10 <- wilcox.test(group_CD$count  ~ group_CD$spray)$p.value)
(g11 <- wilcox.test(group_CE$count  ~ group_CE$spray)$p.value)
(g12 <- wilcox.test(group_CF$count  ~ group_CF$spray)$p.value)

(g13 <- wilcox.test(group_DE$count  ~ group_DE$spray)$p.value)
(g14 <- wilcox.test(group_DF$count  ~ group_DF$spray)$pvalue)

(g15 <- wilcox.test(group_EF$count  ~ group_EF$spray)$p.value)

# a.	Использовать поправку Бонферони
(bonf <- p.adjust(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15), method = "bonferroni"))
#  [1] 1.0000000000 0.0005370756 0.0010896951 0.0004753004 1.0000000000 0.0005370756 0.0009682922 0.0004753004 1.0000000000 0.0371151418 0.7360284385
# [12] 0.0004809812 1.0000000000 0.0004809812

# b.	Использовать поправку Холма
(holm <- p.adjust(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15), method = "holm"))
#  [1] 1.0000000000 0.0004753004 0.0005533098 0.0004753004 1.0000000000 0.0004753004 0.0005533098 0.0004753004 1.0000000000 0.0159064894 0.2628672995
# [12] 0.0004753004 0.6977803544 0.0004753004

alpha <- 0.05 / 15
bonf < alpha
# [1] FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE
holm < alpha
# [1] FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE

# Наименьшие значения p.value у пар AE и BE (по Бонферони) и AC, AE, BC, BE, EF, DE (по Холму).
# Это говорит нам о том, что спреи C и E значительно отличаются по эффективности от остальных.

# c.	Сравните результаты предыдущих двух тестов с тем, что даёт критерий Тьюки
(x <- aov(sprays$count ~ sprays$spray))
# Terms:
#                 sprays$spray Residuals
# Sum of Squares      2668.833  1015.167
# Deg. of Freedom            5        66
#
# Residual standard error: 3.921902
# Estimated effects may be unbalanced
TukeyHSD(x)
#            diff        lwr       upr     p adj
# B-A   0.8333333  -3.866075  5.532742 0.9951810
# C-A -12.4166667 -17.116075 -7.717258 0.0000000
# D-A  -9.5833333 -14.282742 -4.883925 0.0000014
# E-A -11.0000000 -15.699409 -6.300591 0.0000000
# F-A   2.1666667  -2.532742  6.866075 0.7542147
# C-B -13.2500000 -17.949409 -8.550591 0.0000000
# D-B -10.4166667 -15.116075 -5.717258 0.0000002
# E-B -11.8333333 -16.532742 -7.133925 0.0000000
# F-B   1.3333333  -3.366075  6.032742 0.9603075
# D-C   2.8333333  -1.866075  7.532742 0.4920707
# E-C   1.4166667  -3.282742  6.116075 0.9488669
# F-C  14.5833333   9.883925 19.282742 0.0000000
# E-D  -1.4166667  -6.116075  3.282742 0.9488669
# F-D  11.7500000   7.050591 16.449409 0.0000000
# F-E  13.1666667   8.467258 17.866075 0.0000000

# Наибольние значения diff (различий) у пар FC, CB, FE, CA
# Таким образом, результат совпадает с предыдущими тестами ->
# спреи C и E значительно отличаются по эффективности от остальных

# 3.	Используя данные из встроенной таблицы airquality, ответьте на вопрос: в какие месяцы средняя
# температура воздуха в Нью-Йорке отличалась от той, что была в сентябре?
# Уровень значимости: 0.01.

airquality # New York Air Quality Measurements
boxplot(Temp ~ Month, data = airquality)

# Проведем тест Даннетта, указав сравниваемый месяц (сентябрь - 9) и уровень значимости (0.1)
DunnettTest(Temp ~ Month, data = airquality, control="9", conf.level=0.1)
# Dunnett's test for comparing several treatments with a control :
#     95% family-wise confidence level
#
# $`9`
#           diff     lwr.ci    upr.ci    pval
# 5-9 -11.351613 -15.556158 -7.147067 9.6e-10 ***
# 6-9   2.200000  -2.038869  6.438869 0.51101
# 7-9   7.003226   2.798680 11.207771 0.00025 ***
# 8-9   7.067742   2.863196 11.272288 0.00021 ***
#
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Итого, получается, что больше всего темпераьура отличалась от сентябрьской в 5 месяце (в мае), а также
# в 7 и 8 месяцах (июле и августе)




