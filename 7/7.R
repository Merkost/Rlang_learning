# Задания:

# x <- read.delim(pipe("pbpaste"))
df <- read.table('7/xdata.rd', encoding="UTF-8")
factor(df$C.4G)
factor(df$C.6G)
df[df=="ТТ"]<-"TT"
df[df=="ТС"]<-"TC"
df[df=="СТ"]<-"CT"
df[df=="СС"]<-"CC"
df$C.4 <- sub(',', '.', df$C.4)
df$C.6 <- sub(',', '.', df$C.6)
df$C.4 <- sub('87', '8.7', df$C.4)
df$C.6 <- sub('35', '3.5', df$C.6)
df$C.4 <- as.numeric(as.character(df$C.4))
df$C.6 <- as.numeric(as.character(df$C.6))

factor(df$C.4G)
factor(df$C.6G)

plot(factor(df$C.4G), df$C.4)
plot(factor(df$C.6G), df$C.6)

x <- c(174, 162, 188, 192, 165, 168, 172.5)
sex <- c("m", "f", "m", "m", "f", "m", "m")
sex.f <- factor(sex)
s.m <- as.numeric(sex.f)
w <- c(69, 68, 93, 87, 59, 82, 72)
plot(x, w, pch = s.m, col = s.m, cex = 1)
legend("topleft", pch = 1:2, legend = levels(sex.f))




group <- read.table('7/group.rd', header = T)
group$`Имя` <- c("anonymous1", "Анна", "Анастасия", "Ксения", "Антон", "anonymous2", "anonymous3", "anonymous4", "Наталья", "Жеребец", "anonymous5", "Максим", "Кирилл", "Дино", "Никита", "anonymous6", "Илья", "женя", "грэг", "Тахир")
group$`Вес` <- sub(',', '.', group$`Вес`)
group$`Сон`<- sub(',', '.', group$`Сон`)
group$`Вес` <- as.numeric(as.character(group$`Вес`))
group$`Сон` <- as.numeric(as.character(group$`Сон`))

sex.f <- factor(group$`Пол`)
plot(sex.f)
s.m <- as.numeric(sex.f)

plot(group$`Рост`, group$`Вес`, pch = c(24,25)[s.m], col = c('red','light blue')[s.m])
legend("topleft", pch = c(25,24)[s.m], legend = levels(sex.f))


attach(group)
g.mean <- c(mean(`Рост`), mean(`Вес`), mean(`Сон`))
g.mean
g.median <- c(median(`Рост`), median(`Вес`), median(`Сон`))
g.median
g.sd <- c(sd(`Рост`), sd(`Вес`), sd(`Сон`))
g.sd
g.iqr <- c(IQR(`Рост`), IQR(`Вес`), IQR(`Сон`))
g.iqr
detach(group)

summary(group$`Рост`)
quantile(group$`Рост`)
fivenum(group$`Рост`)



# factor(data$C.6G, levels = c("CC", "CT", "TT"))
# utf8ToInt(str(factor(data$C.6G)))

# factor(data$C.6G)