set.seed(123)  

#Variable&Function-Declaration----
BrownSimulate = function(delta, steps) {
  incr = rnorm(steps, mean = 0, sd = sqrt(delta))  
  B = c(0, cumsum(incr))  
  return(B)
}

# Параметры
delta = 0.0001
steps = 1000

# Генерация времени и броуновского движения
tp = seq(0, steps * delta, by = delta)
B = BrownSimulate(delta, steps)

# Визуализация броуновского движения
plot(tp, B, type = "l", col = "blue", lwd = 1,
     main = "Броуновское движение", xlab = "t", ylab = "B(t)")



