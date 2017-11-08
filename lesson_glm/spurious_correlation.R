###spurious correlation 
library(mvtnorm)
library(foreach)
library(doParallel)
library(parallel)
#first part
N = 100; p = 1000
oneSimulation = function(N, p)
{
  X = rnorm(N * p, mean = 0, sd = 1)
  X = matrix(X, nrow = N, ncol = p)

  cor_X = cor(X)
  for (i in 1:p){cor_X[i,i] = 0}
  max(abs(cor_X))
}
cl = makeCluster(4)
registerDoParallel(cl)
result = foreach(n=1:200, .combine = 'c') %dopar% oneSimulation(100, 1000)
hist(result, main = 'maxCorrelation between independent variables\n (N = 100, p = 1000)',
     xlab = 'max correlation')
result = foreach(n=1:200, .combine = 'c') %dopar% oneSimulation(100,5000)
hist(result, main = 'maxCorrelation between independent variables\n (N = 100, p = 5000)',
     xlab = 'max correlation')
stopCluster(cl)
 