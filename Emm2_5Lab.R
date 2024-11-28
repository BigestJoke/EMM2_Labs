#Variable&Function-Declaration----
delta = 0.0001
steps = 1000

S0 = 1
a = 0.5
sigma = 0.9

BrownSimulate = function(delta, steps) {
  incr = rnorm(steps, mean = 0, sd = sqrt(delta))  
  B = c(0, cumsum(incr))  
  return(B)
}
EnsemblePlot = function(n, delta, steps) {
  plot(seq(0, steps * delta, by = delta), numeric(steps + 1), type = "n",
       main = "Ансамбль реализаций броуновского движения", xlab = "t", ylab = "B(t)",)
  for (i in 1:n) {
    Brlz = BrownSimulate(delta, steps)
    lines(seq(0, steps * delta, by = delta), Brlz, 
          col = "blue", lty = 1)
  }
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
GeoEnsemblePlot = function(n, S0, a, sigma, delta, steps) {
  plot(seq(0, steps * delta, by = delta), numeric(steps + 1), type = "n",
       main = "Ансамбль реализаций геометрического броуновского движения",
       xlab = "t", ylab = "S(t)", ylim = c(0, 2))
  
  for (i in 1:n) {
    Brlz = BrownSimulate(delta, steps)  # Генерация броуновского движения
    Srlz = GeoBrSimulation(S0, a, sigma, delta, steps, Brlz)  # Передаем его в GeoBrSimulation
    lines(seq(0, steps * delta, by = delta), Srlz, col = "blue")
  }
}
#BrSimulation----
tp = seq(0, steps * delta, by = delta)
B = BrownSimulate(delta, steps)

#Visualization
plot(tp, B, type = "l", col = "blue", lwd = 1,
     main = "Броуновское движение", xlab = "t", ylab = "B(t)")

#Ensemble
EnsemblePlot(200, delta, steps)

#Confidence intervals
sigma = 3 * sqrt(tp)
lines(tp, sigma, col = "red", lwd = 2, lty = 2)
lines(tp, -sigma, col = "red", lwd = 2, lty = 2)

#Geo-Br-Simulation----
S = GeoBrSimulation(S0,a, sigma, delta, steps,B)

#Visualization
plot(S, type = "l", col = "blue", xlab = "t", ylab = "S(t)", lwd = "1",
     main = "Геометрическое броуновское движение")

#GeoBrEnsemble
GeoEnsemblePlot(200, S0, a, sigma, delta, steps)


