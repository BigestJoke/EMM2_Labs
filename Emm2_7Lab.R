set.seed(123) 

#variable&Function-Declaration----
t = 50
lyambda = 2
n = 1000

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

#insurance-case-modeling----
N_t = InsCaseSim(t, lyambda, n)

#Visualization----
hist(N_t, probability = TRUE, main = "Гистограмма + ф-ция вероятности Пуассона",
     xlab = "N_t", ylim = c(0, 0.05))
xp = 0:max(N_t)
yp = dpois(xp, lambda = lyambda * t)
points(xp, yp, col = "blue", pch = 16)


