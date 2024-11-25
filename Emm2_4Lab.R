set.seed(123)
#Variable&Function-Declaration
rate = 0.01
ib = 1
n = 200

initp = 1
success = 0.8
fail = -0.3
pofsuccess = 0.4

BnFunc = function(initial_balance, rate, per) {
  B = numeric(per + 1)
  B[1] = initial_balance
  for (i in 2:(per + 1)) {
    B[i] = (1 + rate) * B[i - 1]
  }
  return(B)
}
RhoGen = function(per, pofsuccess, success, fail) {
  rhos = numeric(n)
  for (i in 1:n) {
    if (rbinom(1, 1, pofsuccess) == 1) {
      rhos[i] = success  
    } else {
      rhos[i] = fail  
    }
  }
  return(rhos)
}
SnFunc = function(initial_price, rhos) {
  Sn = numeric(length(rhos) + 1)
  Sn[1] = initial_price
  for (i in 2:(length(rhos) + 1)) {
    Sn[i] = (1 + rhos[i - 1]) * Sn[i - 1]
  }
  return(Sn)
}

#Bn----
Bn = BnFunc(ib, rate, n)
plot(0:n, Bn, type = "l", col = "blue", lwd = 2,xlab = "n", ylab = "Bn",
     main = "Рост баланса")


#Sn----
# Генерация случайных доходностей и расчет последовательности
RH = RhoGen(n, pofsuccess, success, fail)
Sn = SnFunc(initp, RH)

# Построение графика
plot(0:n, Sn, type = "l", col = "red", lwd = 2,
     xlab = "n", ylab = "Sn",
     main = "Sn с учетом случайных доходностей")



