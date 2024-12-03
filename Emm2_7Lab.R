#variable&Function-Declaration----
t = 50
lyambda = 2
n = 1000

u0 = 50
tmax = 300
m = 1
cSafely = 3
cRisky = 1.5

tmax2 = 1000
c = 1
lyambda2 = 0.3
m2 = 3
u02 = 100
n2 = 1000

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
CapitalProcSim = function(u0, lyambda, c, m, tmax) {
  capital = u0
  t = 0
  process = list(t = numeric(), capital = numeric())
  
  while (t < tmax) {
    tt = rexp(1, rate = lyambda)  # Время до следующего события
    t = t + tt
    if (t >= tmax) break  # Прекратить, если время превышает максимум
    
    payout = rexp(1, rate = 1 / m)  # Выплата
    capital = capital + c * tt - payout
    process$t = c(process$t, t)  # Сохранить текущее время
    process$capital = c(process$capital, capital)  # Сохранить текущий капитал
  }
  
  process
}
RiskRuin = function(n, u0 ,lyambda,c, tmax, m) {
  count = 0
  for (i in 1:n) {
    capital = u0
    t = 0
    while (t < tmax && capital >= 0) {
      tt = rexp(1, rate = lyambda)
      t = t + tt
      if (t >= tmax) break
      
      payout = rexp(1, rate = 1 / m)
      capital = capital + c * tt - payout
      if (capital < 0) {
        count = count + 1
        break
      }
    }
  }
  count/n
}

#insurance-case-modeling----
N_t = InsCaseSim(t, lyambda, n)

#Visualization----
hist(N_t, probability = TRUE, main = "Гистограмма + ф-ция вероятности Пуассона",
     xlab = "N_t", ylim = c(0, 0.05))
curve(((lyambda * t)^x / factorial(x)) * exp(-lyambda * t),
      add = TRUE, col = "blue", lwd = 2)


#capital-insurance-company----
SafeProc = CapitalProcSim(u0, lyambda, cSafely, m, tmax) #a
Risky = CapitalProcSim(u0, lyambda, cRisky, m, tmax) #b

plot(SafeProc$t, SafeProc$capital, type = "s", col = "darkred",
     main = "Процесс капитала (a)", 
     xlab = "Время", ylab = "Капитал")

plot(Risky$t, Risky$capital, type = "s", col = "darkblue",
     main = "Процесс капитала (b)", 
     xlab = "Время", ylab = "Капитал")

#risk-of-ruin-of-the-company----
ps_ = RiskRuin(n2, u02, lyambda2, c, tmax2, m2)

p = c / (lyambda2 * m2) - 1
checkps = exp(- (1 / m2) * (p / (1 + p)) * u02)

cat("Выборочная вероятность разорения:", ps_, "\n")
cat("Граница вероятности разорения:", checkps, "\n")

