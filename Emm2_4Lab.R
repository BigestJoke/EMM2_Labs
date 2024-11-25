#Variable&Function-Declaration
rate = 0.01
ib = 1
n = 200

BnFunc = function(initial_balance, rate, periods) {
  B = numeric(periods + 1)
  B[1] = initial_balance
  for (i in 2:(periods + 1)) {
    B[i] = (1 + rate) * B[i - 1]
  }
  return(B)
}

#Bn----
Bn = BnFunc(ib, rate, n)
N = 0:n
plot(N, Bn, type = "l", col = "blue", lwd = 2,xlab = "n", ylab = "Bn",
     main = "Рост баланса")



