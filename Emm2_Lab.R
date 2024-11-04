#Libraries----
library(tseries)

#Variable&Function-Declaration----
n1 = 1000
n2 = 1100
a0 = 0.5       
a1 = 0.3 
a2 = 0.2
a3 = 0.1
b1 = 0.4
Garch1 = function(n){
  h = numeric(n)
  sigma = numeric(n)
  h[1] = sqrt(a0 / (1 - a1)) 
  sigma[1] = h[1] * rnorm(1)
  for (t in 2:n) {
    sigma[t] = a0 + a1 * (h[t - 1])^2  
    h[t] = sqrt(sigma[t]) * rnorm(1)    
           
  }
  return(list(sigma = sigma, h = h))
}
Garch3 = function(n) {
  h = numeric(n)
  h[1:3] = sqrt(a0 / (1 - a1 - a2 - a3))  
  for (t in 4:n) {
    h[t] = sqrt(a0 + a1 * h[t - 1]^2 + a2 * h[t - 2]^2 + a3 * h[t - 3]^2) * rnorm(1)
  }
  return(h)  
}
Garch11 = function(n){
  h = numeric(n)
  sigma = numeric(n)
  h[1] = sqrt(a0 / (1 - a1)) 
  sigma[1] = h[1] * rnorm(1)
  for (t in 2:n) {
    sigma[t] = a0 + a1 * (h[t - 1])^2 + b1 * sigma[t-1]  
    h[t] = sqrt(sigma[t]) * rnorm(1)    
    
  }
  return(list(sigma = sigma, h = h))
}
#InitialGarch----
Gch = Garch1(n1)
#Visualization----
par(mfrow = c(2, 1))  
plot(Gch$h, type = "l", col = "blue", 
     main = "Стационарный процесс GARCH(1,0)", 
     xlab = "Время", ylab = "Значение")
plot(Gch$sigma, type = "l", col = "red", 
     main = "Волатильность GARCH(1,0)", 
     xlab = "Время", ylab = "Волатильность")


#MNK----
hSquared = Gch$h^2
laghSquared = c(NA, hSquared[-n1])  
validData = na.omit(data.frame(hSquared = hSquared[-1], 
                                laghSquared = laghSquared[-1]))

modelMnk = lm(hSquared ~ laghSquared, data = validData)
summary(modelMnk)
a0mnk = coef(modelMnk)[1]
a1mnk = coef(modelMnk)[2]

cat("Оценка a0:", a0mnk, "\n")
cat("Оценка a1:", a1mnk, "\n")


#Garch()----
startVal = c(a0,a1)  
modelGch = garch(Gch$h, order = c(0, 1), start = startVal)
summary(modelGch)

a0gch = coef(modelGch)[1]  
a1gch = coef(modelGch)[2] 
cat("Оценка a0:", a0gch, "\n")
cat("Оценка a1:", a1gch, "\n")

#InitialGarch(3,0)----
Gch3 = Garch3(n2)
trainSize = floor(10/11 * n2)
trainData = Gch3[1:trainSize]
testData = Gch3[(trainSize + 1):n2]
testsize = length(testData)

startVal2 = list(a0,a1,a2,a3)
modelGch2 = garch(trainData, order = c(0, 3), start = startVal2)
summary(modelGch2)

a0gch3 = coef(modelGch2)[1]  
a1gch3 = coef(modelGch2)[2]  
a2gch3 = coef(modelGch2)[3]  
a3gch3 = coef(modelGch2)[4] 

cat("a0: ",a0gch3,"\n","a1: ",a1gch3,"\n","a2: ",a2gch3,"\n","a3: ",a3gch3,"\n")

#Forecast----
hFor = numeric(testsize) 
for (t in seq_along(hFor)) {
  i = trainSize + t  
  hPrev1 = Gch3[i - 1]^2; hPrev2 = Gch3[i - 2]^2 
  hPrev3 = Gch3[i - 3]^2  
  hFor[t] = a0gch3 + a1gch3 * hPrev1 + a2gch3 * hPrev2 + a3gch3 * hPrev3
}

#VisualizationTest----
testIndices = (trainSize + 1):(trainSize + length(hFor))
par(mfrow = c(1, 1))
plot(testIndices, testData, type = "l", col = "blue", 
     main = "Тестовая выборка и прогнозы GARCH(3,0)", 
     xlab = "Наблюдения", ylab = "Значение")
lines(testIndices, hFor, col = "red")

#InitialGarch11----
Gch11 = Garch11(n1)
#VisualizationGarch11----
par(mfrow = c(2, 1))  
plot(Gch11$h, type = "l", col = "blue", 
     main = "Стационарный процесс GARCH(1,0)", 
     xlab = "Время", ylab = "Значение")
plot(Gch11$sigma, type = "l", col = "red", 
     main = "Волатильность GARCH(1,0)", 
     xlab = "Время", ylab = "Волатильность")

startVal11 = list(a0,a1,b1)
modelGch11 = garch(Gch11$h, order = c(1, 1), start = startVal11)
a0gch11 = coef(modelGch11)[1]  
a1gch11 = coef(modelGch11)[2] 
b1gch11 = coef(modelGch11)[3]
cat("Оценка a0:", a0gch11, "\n")
cat("Оценка a1:", a1gch11, "\n")
cat("Оценка b1:", b1gch11, "\n")

