set.seed(123)  

#Variable&Function-Declaration----
delta = 0.0001
steps = 1000

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



