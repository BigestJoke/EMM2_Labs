set.seed(123) 

#variable&Function-Declaration----
t = 50
lyambda = 2
n = 1000

u0 = 50
t_max <- 100
mu <- 1
c_safe <- 3
c_risky <- 1.5

InsCaseSim = function(t, lyambda, n) {
  N_t = numeric(n)
  for (i in 1:n) {
    TT = 0
    count = 0
    while (TT < t) {
      TT = TT + rexp(1, rate = lyambda)
      if (TT < t) count = count + 1
    }
    
    N_t[i] = count
  }
  N_t
}
simulate_capital_process <- function(U0, c, lambda, mu, t_max) {
  capital <- U0
  times <- 0
  process <- list(times = numeric(), capital = numeric())
  
  while (times < t_max) {
    tau <- rexp(1, rate = lambda)
    times <- times + tau
    if (times >= t_max) break
    
    payout <- rexp(1, rate = 1 / mu)
    capital <- capital + c * tau - payout
    process$times <- c(process$times, times)
    process$capital <- c(process$capital, capital)
  }
  
  process
}

#insurance-case-modeling----
N_t = InsCaseSim(t, lyambda, n)

#Visualization----
hist(N_t, probability = TRUE, main = "Гистограмма + ф-ция вероятности Пуассона",
     xlab = "N_t", ylim = c(0, 0.05))
curve(((lyambda * t)^x / factorial(x)) * exp(-lyambda * t),
      add = TRUE, col = "blue", lwd = 2)





