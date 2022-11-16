salary <- c(21, 19, 27, 11, 102, 25, 21)
salary
mean(salary)
CI <- quantile(salary, prob=c(0.95,0.99))
CI
boot_np <- function(data , Nboot=5000) {
  boots <- numeric(Nboot) # Пустой вектор для хранения результатов

  for (i in 1:Nboot) {
    boots[i] <- mean(sample(data,replace=T))
  }

  CI <- quantile(boots, prob=c(0.95,0.99))
  return (c(m = mean(data),CI))
}
boot_np(salary)