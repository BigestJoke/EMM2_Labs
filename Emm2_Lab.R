#Libraries
library(tseries)

#Variable&Function-Declaration----
n = 1000        
a0 = 0.1        
a1 = 0.5        
Garch = function(n){
  h = numeric(n)
  sigma = numeric(n)
  h[1] = sqrt(a0 / (1 - a1)) 
  sigma[1] = h[1] * rnorm(1)
  for (t in 2:n) {
    h[t] <- sqrt(a0 + a1 * h[t - 1]^2)  
    sigma[t] <- h[t] * rnorm(1)        
  }
}

#Visualisation
par(mfrow = c(2, 1))  
plot(sigma, type = "l", 
     col = "blue", main = "Стационарный процесс GARCH(1,0)", 
     xlab = "Время", ylab = "Значение")
plot(h, type = "l", col = "red", 
     main = "Волатильность GARCH(1,0)", 
     xlab = "Время", 
     ylab = "Волатильность")

#MNK

# МНК оценка для GARCH(1,0)
y = sigma^2
x = cbind(1, h[-n]^2)  # матрица предикторов (константа и лагированное значение h^2)

# Регрессия для получения оценок
fit <- lm(y[-1] ~ x)  # исключаем первое значение, т.к. нет значения для h_{n-1}
summary(fit)

# Извлечение оценок параметров
a0_hat <- coef(fit)[1]
a1_hat <- coef(fit)[2]
cat("Оценка a0:", a0_hat, "\n")
cat("Оценка a1:", a1_hat, "\n")


