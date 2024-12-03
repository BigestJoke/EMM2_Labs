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

