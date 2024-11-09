# Установка необходимых пакетов
if (!require("tseries")) install.packages("tseries", dependencies = TRUE)
if (!require("rugarch")) install.packages("rugarch", dependencies = TRUE)
library(tseries)
library(rugarch)

#Variable&FunctionDeclaration----
n = 2100
ARpar = c(-0.3, 0.4)  
ARCHpar = c(1, 0.2, 0.1, 0.2)  
ARARCH = function(n){
  x = numeric(n)
  variance = numeric(n)
  noise = rnorm(n,0,1)
  variance[1:3] = var(noise)
  x[1:3] = rnorm(3)
  for (t in 4:n) {
    variance[t] = ARCHpar[1] + ARCHpar[2] * x[t-1]^2 +
                  ARCHpar[3] * x[t-2]^2 +ARCHpar[4] * x[t-3]^2
    x[t] = ARpar[1] * x[t-1] + ARpar[2] * x[t-2] + sqrt(variance[t]) * noise[t]
  }
  return(list(variance = variance, x = x))
}

#ARARCH&Visualization----
AArch = ARARCH(n)
plot(AArch$x, type = "l", col = "darkred", main = "Генерированный процесс AR(2)ARCH(3)",
     xlab = "Время", ylab = "Значение x")
