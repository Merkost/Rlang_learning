###### lecture 10 ###########################################################
library(HSAUR2)
library(car)
library(sm)
data(weightgain)
#лабораторным крысам примерно одинакового возраста и веса в течение
#определенного времени давали корм с разным содержанием белка (фактор type с двумя уровнями:
#низкое содержание - Low, и высокое содержание - High). Кроме того, корма различались
#по происхождению белка (фактор source с двумя уровнями: beef - говядина,
# и cereal - злаки). В конце эксперимента был измерен прирост веса у крыс в каждой группе (weightgain)
str(weightgain)

boxplot(weightgain ~ source*type, data=weightgain,
        varwidth=TRUE, col=c("gold","darkgreen"),
        main="прирост веса в зависимости от характеристик корма",
        xlab="тип питания")

two.fact<-c(rep("BL",10),rep("BH",10),rep("CL",10),rep("CH",10))
tapply(weightgain[,3], two.fact, mean)
tapply(weightgain[,3], two.fact, sd)

plot.design(weightgain)
with(weightgain, interaction.plot(x.factor = type,
                                  trace.factor = source,
                                  response = weightgain))
M1 <- aov(weightgain ~ source + type + source:type,
          data = weightgain)
summary(M1)

file.show("Lectures/class_A.txt")
cl.A<-read.table("Lectures/class_A.txt", sep=" ")
summary(cl.A)
colnames(cl.A)<-c("имя","вес","рост","пол")

as.factor(cl.A$пол)
as.numeric(as.factor(cl.A$пол))

attach(cl.A)
plot(вес, рост, col=as.numeric(as.factor(пол)), pch=as.numeric(as.factor(пол)))
legend("topleft", pch=1:2, col=1:2, legend=c("девочки", "мальчики"))

library(car) # д.и. qqPlot()
library(sm) # д.и. sm.density()
par(mfrow=c(2,2))
qqPlot(вес, dist= "norm", pch=18, xlab="Квантили нормального распределения",
       ylab="Наблюдаемые квантили", main="Вес")
sm.density(вес, model = "Normal", xlab="Имитированная выборка",
           ylab="Функция плотности распределения")
qqPlot(рост, dist= "norm", pch=18, xlab="Квантили нормального распределения",
          ylab="Наблюдаемые квантили", main="Рост")
sm.density(рост, model = "Normal", xlab="Имитированная выборка",
              ylab="Функция плотности распределения")
par(mfrow=c(1,1))

shapiro.test(вес)
shapiro.test(рост)

cor(вес, рост)
cor.test(вес, рост)
cor.test(вес, рост, method = "spearman")
cor.test(вес, рост, method = "kendall")

detach(cl.A)

cor(trees)
cor(longley)
symnum(cor(longley))

cor.l <- cor(longley)
image(1:ncol(cor.l), 1:nrow(cor.l), cor.l,col=topo.colors(8), axes=FALSE, xlab="", ylab="")
axis(1, at=1:ncol(cor.l), labels=abbreviate(colnames(cor.l)))
axis(2, at=1:nrow(cor.l), labels=abbreviate(rownames(cor.l)), las = 2)

boxplot(вес ~ пол, data=cl.A, notch=T,varwidth=TRUE, col=c("yellow", "blue"),
                xlab="пол")

###### lecture 11 ###########################################################
#### КАЧЕСТВЕННЫЕ ДАННЫЕ ####################################################
# в среднем по больнице доля курильщиков 70%, а в отделении патологии лёгких
# 356 из 476 человек. Отличается ли доля в этом отделении?
binom.test(x=356, n=476, p=0.7, alternative="two.sided")

qchisq(p = 0.95, df = 1)
mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = TRUE)
chisq.test(mice)

ftable(Titanic, row.vars = 1:3)
titanic <- apply(Titanic, c(1, 4), sum)
titanic
mosaicplot(titanic, col = c("red", "green"), main = "",
           cex.axis=1)
chisq.test(titanic)

#точный тест Фишера
num<-factorial(9)*factorial(14)*factorial(11)*factorial(12)/factorial(23)
denum1<-factorial(1)*factorial(8)*factorial(10)*factorial(4)
num/denum1
denum2<-factorial(0)*factorial(9)*factorial(11)*factorial(3)
num/denum2

(X <- matrix(c(1, 10, 8, 4), ncol = 2))
fisher.test(X, alternative = "less") # односторонний
fisher.test(X) #  alternative = "two.sided"
