#Типы данных

#Числовой вектор
x <- c(174, 162, 188, 192, 165, 168, 172.5)
str(x)
is.vector(x)
is.numeric(x)

#Номинальные данные (шкала наименований)
sex <- c("m", "f", "m", "m", "f", "m", "m")
str(sex)
is.character(sex)
table(sex)
sex.f <- factor(sex)
sex.f
plot(sex.f)
is.factor(sex.f)
str(sex.f)
sex.f[5:6]
sex.f[6:7]
sex.f[6:7, drop = T]

# Задача:
# В файле данные содержатся реальные данные (AGCT)
# убрать русские буквы, убрать опечатки и отрисовать.

s.m <- as.numeric(sex.f)
w <- c(69, 68, 93, 87, 59, 82, 72)
# Задача:
# Построить график (рост, вес, категория) + по данным группы
plot(x, w, pch = s.m, col = s.m)
legend("topleft", pch = 1:2, legend = levels(sex.f))


#Упорядочивание факторов
m <- c("L", "S", "XL", "XXL", "S", "M", "L")
m.f <- factor(m)
m.f
m.o <- ordered(m.f, levels = c("S", "M", "L", "XL", "XXL"))
m.o
a <- factor(3:5)
a
as.numeric(a)
as.numeric(as.character(a))

# Пропущенные данные (NA)
h <- c(8, 10, NA, NA, 8, NA, 8)
h

# Задача, кто сколько спит из группы
mean(h)
mean(h, na.omit(h))
mean(h, na.rm = T)
h[is.na(h)] <- mean(h, na.rm = T)

# Выбросы
# max 18.5
# min .19
# table AaA

# Диаграмма размаха или "ящик с усами"
salary <- c(21, 19, 27, 11, 102, 25, 21)
m.s <- median(salary)
r.s <- IQR(salary)
salary2 <- c(salary, sample((m.s - r.s):(m.s + r.s), 1000, replace = T))
boxplot(salary2, log = "y")
boxplot(trees)

# Создать таблицу данных
d <- data.frame(weight = w, height = x, size = m.o, sex = sex.f)
# Имена строк <- имена студентов
attach(d)
mean(height)
var(weight)
detach

# Посмотреть разницу на выходе
summary(d)
quantile(d)
fivenum(d)



# Тестирование
t.test(salary, nu = mean(salary))

wilcox.test(salary2, mu = median(salary2), conf.int = T)

# Тесты на нормальность
# 1. Шапира-Уилкса
shapiro.test(salary)
set.seed(1638)
shapiro.test(rnorm(100))
# p.value = 0.9094  => Ho норм.

# 2. Графический способ
qqnorm(salary2)  # Квантили выборочные
qqline(salary2, cd = 2) # Квантили теоретические
# Визуально не похоже на нормальное распределение

# 3. Универсальный тест Колмогорова - Смирнова
ks.test(salary2, "pnorm")
