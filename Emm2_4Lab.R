#Variable&Function-Declaration
rate = 0.01
ib = 1
n = 200

initp = 1
success = 0.8
fail = -0.3
pofsuccess = 0.4

S0 <- 100
K <- 100
N <- 10
a <- -0.3
b <- 0.8
r <- 0.2

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

BinomCoeff = function(n, k) {
  choose(n, k)
}
BinomialSum = function(j, N, p) {
  s = 0
  for (k in j:N) {
    s = s + BinomCoeff(N, k) * p^k * (1 - p)^(N - k)
  }
  return(s)
}
CalculateCallPrice = function(S0, K, N, a, b, r) {
  p_ = (r - a) / (b - a)
  p_s = ((1 + b) / (1 + r)) * p_
  K0 = 1 + floor(log(K / (S0 * (1 + a)^N)) / log((1 + b) / (1 + a)))
  
  if (K0 <= N) {
    price = S0 * BinomialSum(K0, N, p_s) - 
      K * (1 + r)^(-N) * BinomialSum(K0, N, p_)
  } else {
    price = 0
  }
  return(price)
}
#Bn----
Bn = BnFunc(ib, rate, n)
plot(0:n, Bn, type = "l", col = "blue", lwd = 2,xlab = "n", ylab = "Bn",
     main = "Рост баланса")


#Sn----
RH = RhoGen(n, pofsuccess, success, fail)
Sn = SnFunc(initp, RH)
plot(0:n, Sn, type = "l", col = "red", lwd = 2,xlab = "n", ylab = "Sn",
     main = "Sn с учетом случайных доходностей")


#Call-Price----
callPrice = CalculateCallPrice(S0, K, N, a, b, r)
callPrice

