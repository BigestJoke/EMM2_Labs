set.seed(123) 

#variable&Function-Declaration----
t = 50
lyambda = 2
n = 1000

u0 = 50
tmax = 300
m = 1
cSafely = 3
cRisky = 1.5

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
CapitalProcSim = function(u0, c, lyambda, m, tmax) {
  capital = u0
  t = 0
  process = list(times = numeric(), capital = numeric())
  
  while (t < tmax) {
    tt = rexp(1, rate = lyambda)
    t = t + tt
    if (t >= tmax) break
    
    payout = rexp(1, rate = 1 / m)
    capital = capital + c * tt - payout
    process$t = c(process$t, t)
    process$capital = c(process$capital, capital)
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


#capital-insurance-company----
SafeProc = CapitalProcSim(u0, lyambda,cSafely, m, tmax) #a
Risky = CapitalProcSim(u0, lyambda,cRisky, m, tmax) #b

plot(process_safe$t, process_safe$capital, type = "s", col = "blue",
     main = "Процесс капитала A)", 
     xlab = "Время", ylab = "Капитал")

plot(process_risky$t, process_risky$capital, type = "s", col = "red",
     main = "Процесс капитала B)", 
     xlab = "Время", ylab = "Капитал")

