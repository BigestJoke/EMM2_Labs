#Variable&FunctionDeclataration----
n=700;
o1= 0.3; o2=1; o3=1.2
n1 = 1000; o4 = 0.4 #for 4 task
AR = function(n,o){
  x = numeric(n) 
  x[1] = rnorm(1,0,1)   
  for (i in 2:n) 
    {
    e = rnorm(1,0,1) 
    x[i] = o * x[i - 1] + e 
    }
 x
}
RootFind = function(ar){
  Upsum=0
  Downsum=0
  for (i in 2:n) 
  {
  Upsum = Upsum + (ar[i-1] * ar[i]); 
  Downsum = Downsum + (ar[i-1])^2    
  }
  root = Upsum/Downsum #Производная, приравненная к 0
}
SquareSum = function(ar, o) {
  totalSum = 0 
  for (i in 2:n)
  {
    totalSum = totalSum + (ar[i] - o * ar[i-1])^2
  }
  totalSum
}
PartialMNK = function(ar, n1) {
  needpar = n1 - 9
  vecofMNK = numeric(needpar)
  Upsum = 0
  Downsum = 0
  j = 1
  for (k in 10:n1) {
    for (i in 2:k) {
      Upsum = Upsum + (ar[i - 1] * ar[i]) 
      Downsum = Downsum + (ar[i - 1])^2   
    }
    root = Upsum / Downsum 
    vecofMNK[j] = root
    j = j + 1
  }
  return(vecofMNK)
}
Ar2Generation = function(n, o1, o4) {
  ar = numeric(n)
  ar[1:2] = rnorm(2,0,1)
  for (i in 3:n) {
    ar[i] = o1 * ar[i-1] + o4 * ar[i-2] + rnorm(1,0,1)
  }
  return(ar)
}
checkStac = function(o1, o4) {
  D = o1^2 - 4 * o4
  if (D < 0) {
    Lvalid = o1/2
    Limaginary = sqrt(abs(D))/2
    mod = sqrt(Lvalid^2 + Limaginary^2)
    Stac = mod < 1
  } else {
    L1 = (-o1 + sqrt(D))/2  
    L2 = (-o1 - sqrt(D))/2  
    Stac = abs(L1) < 1 && abs(L2) < 1
  }
  return(Stac)
}
#GetStart----
ar1 = AR(n,o1)
ar2 = AR(n,o2)
ar3 = AR(n,o3)

#visualisation----
plot(ar1, type = 'h', col="blue", xlab= "i")
plot(ar2, type = 'h',col="red",xlab= "i")
plot(ar3, type = 'h',xlab= "i")
#LSM----
o1root = RootFind(ar1)
#MaximumLikelihood
total = optimize(SquareSum, interval = c(-10, 10), ar = ar1)
o1opt = total$minimum

if (round(o1root,5)  == round(o1opt,5)) 
{
  print("its right")
}

#ModWithNewPar----
ar4 = AR(n1,o4)
VMV = PartialMNK(ar4,n1)
plot (VMV, type = 'h', col= "lightblue",xlab= "i")


#AR(2)----
ar5 = Ar2Generation(n1,o1,o4)
if (checkStac(o1, o4)) {
 print ("There is stationary")
} else {
print ("There isn't stationary)")
}
plot (ar5, type = 'h', col= "green",xlab= "i")
