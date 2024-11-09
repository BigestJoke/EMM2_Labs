#Libraries
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
Forecasts = function(x,testN,thetaEst,ARCHest){
  forecastX = numeric(testN)
  forecastVariance = numeric(testN)
  
  for (i in 1:testN) {
    newi = trainN + i
    preVal = c(x[newi-1], x[newi-2])
    forecastX[i] = sum(thetaEst * preVal)
    forecastVariance[i] = sqrt(ARCHest[1] + ARCHest[2] * x[newi-1]^2 + 
                          ARCHest[3] * x[newi-2]^2 + ARCHest[4] * x[newi-3]^2)
  }
  return(list(forecastX = forecastX,forecastVariance = forecastVariance))
}

#ARARCH&Visualization----
AArch = ARARCH(n)
plot(AArch$x, type = "l", col = "darkred", main = "AR(2)ARCH(3)",
     xlab = "t", ylab = "x")

#Test&Train&Visualization
trainN = floor(20/21 * n)
testN = n - trainN

train = AArch$x[1:trainN]
test = AArch$x[(trainN + 1):n]

plot(train, type = "l", col = "darkblue", main = "Обучающая выборка", 
     xlab = "t", ylab = "train")
plot(test, type = "l", col = "black", main = "Тестовая выборка", 
     xlab = "t", ylab = "test")



#ParEstimation----
ARest = arima(train, order = c(2, 0, 0))
thetaEst = coef(ARest)[1:2]
cat("Оцененные параметры для AR(2):", thetaEst, "\n")

ARresid = residuals(ARest)
plot(ARresid, type = "l", col = "black", main = "Остатки AR(2)",
     xlab = "t", ylab = "Остатки")

ARCHMod = garch(ARresid, order = c(0, 3))
ARCHest = coef(ARCHMod)
cat("Оцененные параметры для ARCH(3):", ARCHest, "\n")

Varest = fitted(ARCHMod)[,1]^2
plot(Varest, type = "l", col = "red", main = "Оцененная дисперсия ARCH(3)",
     xlab = "t", ylab = "Дисперсия")


#Forecasts&Visualization----
Fcast = Forecasts(AArch$x,testN,thetaEst,ARCHest)
upper = Fcast$forecastX + Fcast$forecastVariance
lower = Fcast$forecastX - Fcast$forecastVariance

plot(test, type = "l", col = "blue", main = "Прогноз на 1 шаг вперед",
     xlab = "i", ylab = "value", 
     ylim = range(c(lower, upper, test)))
points(Fcast$forecastX, col = "black", pch = 20)
lines(upper, col = "red")
lines(lower, col = "red")


#Finance----
data = read.csv("GAZP.csv", sep = ";")

plot(data$X.TIME, data$X.CLOSE, type = "l", col = "red", 
     main = "График динамики актива (CLOSE)", 
     xlab = "Время", ylab = "Цена закрытия")



returnsLog = diff(log(data$X.CLOSE))
plot(returnsLog, type = "l", col = "green", 
     main = "Доходность актива по Log Diff", 
     xlab = "Дата", ylab = "returnsLog")
