#Variable&FunctionDeclataration----
n=700;
o1= 0.3; o2=1; o3=1.2
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
