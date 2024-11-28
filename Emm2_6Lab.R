#Variable&Function-Declaration----
S0 = 100
a = 0.5
sigma = 0.8
delta = 0.01
steps = 1000

BrownSimulate = function(delta, steps) {
  incr = rnorm(steps, mean = 0, sd = sqrt(delta))  
  B = c(0, cumsum(incr))  
  return(B)
}
GeoBrSimulation = function(S0,a, sigma, delta, steps,B){
  S = numeric(steps + 1)  
  S[1] = S0  # устанавливаем начальное значение
  
  for (i in 1:steps) {
    # Используем значения броуновского движения из предыдущего кода
    S[i + 1] = S0 * exp((a - sigma^2 / 2) * (i * delta) + sigma * B[i + 1])  
  }
  return(S)
}

#Br-Geo-BrSimulation----
tp = seq(0, steps * delta, by = delta)
B = BrownSimulate(delta, steps)
S = GeoBrSimulation(S0,a, sigma, delta, steps,B)

#Visualization
plot(S, type = "l", col = "blue", xlab = "t", ylab = "S(t)", lwd = "1",
     main = "Геометрическое броуновское движение")



  