salary <- c(21, 19, 27, 11, 102, 25, 21)
salary
mean(salary)
boot_np <- function(data , Nboot=5000) {
  boots <- numeric(Nboot) # Пустой вектор для хранения результатов

  for (i in 1:Nboot) {
    boots[i] <- mean(sample(data,replace=T))
  }
  print(boots)

  CI <- quantile(boots, prob=c(0.95,0.99)) #Confidence interval
  return (c(m = mean(data),CI))
}
boot_np(salary)
param_CI <- function(data) {
  n <- length(data)
  m <- mean(data)
  SE <- sd(data)/sqrt(n) #Standard Error
  E <- qt(c(0.95,0.99), df=n-1) * SE
  # return (E)
  CI <- m + c(-E, E) #Confidence interval
  return (c(m, CI))
}
param_CI(salary)

jknife <- function(x, f, ci=0.95) {
    theta <- f(x)
    n <- length(x)
    partials <- rep(0,n)  #Empty vector for saving fxn
    for (i in 1:n){
       partials[i] <- f(x[-i])
    }
    pseudos <- (n*theta) - (n-1)*partials
    jack.est <- mean(pseudos)
    jack.se <- sqrt(var(pseudos)/n)
    alpha <- 1-ci
    CI <- qt(alpha/2,n-1,lower.tail=FALSE)*jack.se
    jack.ci <- c(jack.est - CI, jack.est + CI)
    return (list(est=jack.est, se=jack.se, ci=jack.ci, pseudos = pseudos, partials=partials))
}
jack.means <- jknife(salary, mean)
jack.means
jack.vars <- jknife(salary, var)
jack.vars



df <- read.table('8/data.rd', encoding="UTF-8")
healthy <- df[grepl('c', df$Nn), ]
ill <- df[!grepl('c', df$Nn),]
print(sprintf("C.4 healthy and ill: %f %f", mean(healthy$C.4),mean(ill$C.4)))
print(sprintf("C.6 healthy and ill: %f %f", mean(healthy$C.6),mean(ill$C.6)))

# name <- c("C.4", "C.6")
# healthy_people <- c(mean(healthy$C.4),mean(healthy$C.6))
# ill_people <- c(mean(ill$C.4),mean(ill$C.6))
# df.stats <- data.frame(name, healthy_people, ill_people)

healthy$C.4 - ill$C.4 # Индивидуальные разности
mean(healthy$C.4 - ill$C.4)

t.test(healthy$C.4, head(ill$C.4, 60), paired = TRUE)
t.test(healthy$C.6, head(ill$C.6, 60), paired = TRUE)
t.test(healthy$C.6, ill$C.6)


