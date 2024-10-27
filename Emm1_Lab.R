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
RootFind = function(o1){
  total_sum = 0
  for (i in 2:n) 
  {
    total_sum = total_sum + 2 * (ar1[i] - o1 * ar1[i-1]) * (-ar1[i-1])
  }
  total_sum
}
ar1 = AR(n,o1)
ar2 = AR(n,o2)
ar3 = AR(n,o3)

#visualisation----
plot(ar1, type = 'h', col="blue", xlab= "i")
plot(ar2, type = 'h',col="red",xlab= "i")
plot(ar3, type = 'h',xlab= "i")
#LSM----
result = uniroot(RootFind, interval = c(-10, 10),tol = 1e-10)
root_value = result$root
#MaximumLikelihood
