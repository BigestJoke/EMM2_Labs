#Variable&Function-Declaration----
S0 = 100
a = 0.5
sigma = 0.8
delta = 0.0001
steps = 1000

BrownSimulate = function(delta, steps) {
  incr = rnorm(steps, mean = 0, sd = sqrt(delta))  
  B = c(0, cumsum(incr))  
  return(B)
}
GeoBrSimulation = function(S0,a, sigma, delta, steps,B){
  S = numeric(steps + 1)  
  S[1] = S0  
  
  for (i in 1:steps) {
    S[i + 1] = S0 * exp((a - sigma^2 / 2) * (i * delta) + sigma * B[i + 1])  
  }
  return(S)
}
IncrCalc = function(S) {
  diff(log(S)) 
}
ParEst = function(increments, delta) {
  m_ = mean(increments)
  D = var(increments)
  
  sigma_ = D / delta  
  a_ = m_ / delta + sigma_ / 2  
  
  return(list(a_ = a_, sigma_ = sigma_))
}

#Br&GeoBrSimulation----
tp = seq(0, steps * delta, by = delta)
B = BrownSimulate(delta, steps)
S = GeoBrSimulation(S0,a, sigma, delta, steps,B)

#Visualization
plot(S, type = "l", col = "blue", xlab = "t", ylab = "S(t)", lwd = "1",
     main = "Геометрическое броуновское движение")

increments = IncrCalc(S)
params = ParEst(increments, delta)

#Output
cat("Оценка параметра a :", params$a_, "\n")
cat("Оценка параметра sigma:", params$sigma_, "\n")

  