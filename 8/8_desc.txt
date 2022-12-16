# Задание 8

# 8.1.	Найдите параметрические и бутстрепные доверительные интервалы (95% и 99%)
# для средней зарплаты из выборки salary = {21, 19, 27, 11, 102, 25, 21};

salary <- c(21, 19, 27, 11, 102, 25, 21)
salary
mean(salary)
# Функция нахождения бутстрепных доверительных интервалов.
boot_np <- function(data, Nboot = 5000) {
  boots <- numeric(Nboot) # Пустой вектор для хранения результатов

  for (i in 1:Nboot) {
    boots[i] <- mean(sample(data, replace = T))
  }
  print(boots)

  CI <- quantile(boots, prob = c(0.95, 0.99)) #Confidence interval
  return(c(m = mean(data), CI))
}

# бутстрепные доверительные интервалы
boot_np(salary)

# Функция нахождения параметрических доверительных интервалов.
param_CI <- function(data) {
  n <- length(data)
  m <- mean(data)
  SE <- sd(data) / sqrt(n) #Standard Error
  E <- qt(c(0.95, 0.99), df = n - 1) * SE
  # return (E)
  CI <- m + c(-E, E) #Confidence interval
  return(c(m, CI))
}

# параметрические доверительные интервалы
param_CI(salary)

# 8.2.	Напишите функцию с реализацией метода «складного ножа» (jackknife):
# найдите оценку средней зарплаты и выборочной дисперсии этим методом

# Функция с реализацией метода «складного ножа».
jknife <- function(x, f, ci = 0.95) {
  theta <- f(x)
  n <- length(x)
  partials <- rep(0, n)  #Пустой вектор для дальнейшего сохранения значений функции
  for (i in 1:n) {
    partials[i] <- f(x[-i])
  }
  pseudos <- (n * theta) - (n - 1) * partials
  jack.est <- mean(pseudos)
  jack.se <- sqrt(var(pseudos) / n)
  alpha <- 1 - ci
  CI <- qt(alpha / 2, n - 1, lower.tail = FALSE) * jack.se # Распределение Стьюдента
  jack.ci <- c(jack.est - CI, jack.est + CI)
  return(list(est = jack.est, se = jack.se, ci = jack.ci, pseudos = pseudos, partials = partials))
}

# Находим оценку средней зарплаты и выборочной дисперсии этим методом
jack.means <- jknife(salary, mean)
jack.means
jack.vars <- jknife(salary, var)
jack.vars

# 8.3.	Для данных из файла данные.xls проверьте гипотезы (используйте t-критерий Стьюдента с поправкой Уэлча):
# a.	Среднее C-4 не отличается для здоровых пациентов (Nn=c1-c60) и пациентов с диагнозом;
# b.	Среднее C-6 не отличается для здоровых пациентов (Nn=c1-c60) и пациентов с диагнозом;

# Распределяем больных и здоровых из датасета
df <- read.table('8/data.rd', encoding = "UTF-8")
healthy <- df[grepl('c', df$Nn),]
ill <- df[!grepl('c', df$Nn),]
print(sprintf("C.4 healthy and ill: %f %f", mean(healthy$C.4), mean(ill$C.4)))
print(sprintf("C.6 healthy and ill: %f %f", mean(healthy$C.6), mean(ill$C.6)))

healthy$C.4 - ill$C.4 # Индивидуальные разности
mean(healthy$C.4 - ill$C.4)

t.test(healthy$C.4, head(ill$C.4, 60), paired = TRUE)
t.test(healthy$C.6, head(ill$C.6, 60), paired = TRUE)
t.test(healthy$C.6, ill$C.6)
# существует статистическая разница между двумя средними значениями.


