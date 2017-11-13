

##packages loading
library(mvtnorm)





#ridge regression
#input: y, X, lambda; output: coef, predictfunction

ridgeRegression = function(y, X, lambda)
{
  beta_ridge = solve(t(X) %*% X + lambda * diag(1, dim(X)[2])) %*% t(X) %*% y
  ridgePredict = function (X){return (X %*% beta_ridge)}
  result = list(lambda = lambda, coef_ridge = beta_ridge, modelPredict = ridgePredict)
  return(result)
}



#cross validation 
#input: y, X, lambda, fold, criterion, randomseed; output: criterion value, figure, max point


computeOneFold = function(reindex, y_fun, X_fun, lambda = 0, fun)
{
  fitmodel = fun(y = y_fun[-reindex], X = X_fun[-reindex,], lambda=lambda)
  y_pre = fitmodel$modelPredict(X = X_fun[reindex,])
  rss = sum((y_fun[reindex] - y_pre)^2)
  return(rss)
}
getFolddivide = function (samplesize,fold = 5, randomseed = NULL)
{
  #divide into folds
  if(!is.null(randomseed)) set.seed(randomseed)
  reindex=sample(1:samplesize, size=samplesize, replace = F)
  foldsize = samplesize %/% fold
  folddivide = list()
  for (f in 1:fold)
  {
    folddivide = c(folddivide, list(reindex[((f-1)*foldsize+1):(f*foldsize)]))
  }
  return(folddivide)
}

getCV = function(lambda = 0, y_fun, X_fun, fun, fold = 5, folddivide = NULL, randomseed = NULL)
{
  if (is.null(folddivide)) folddivide = getFolddivide(length(y_fun), fold = fold, randomseed = randomseed)
  rss = sum(sapply(folddivide,FUN = computeOneFold, y_fun=y_fun, X_fun = X_fun, lambda=lambda, fun=fun))
  N = length(y_fun)
  eigenvalue = eigen(t(X_fun)%*%X_fun)$values
  df =  sum(eigenvalue / (eigenvalue + lambda))
  mse = rss / N
  aic = N * log(rss / N) + 2 * df
  bic = N * log(rss / N) + log(N) * df
  return(c(lambda = lambda, MSE = mse, AIC = aic, BIC = bic))
}

LambdaChoosing = function(lambdaseq, y_fun, X_fun, fun,fold = 5, randomseed = NULL)
{
  folddivide = getFolddivide(length(y_fun),fold = fold, randomseed = randomseed)
  result = t(sapply(lambdaseq, FUN = getCV,
                    y_fun = y_fun, X_fun = X_fun, folddivide = folddivide, fun = fun))
  par(mfrow = c(3,1))
  plot(lambdaseq, result[,2], main = 'MSE', ylab = 'mse',type='l')
  plot(lambdaseq, result[,3], main = 'AIC', ylab = 'aic',type='l')
  plot(lambdaseq, result[,4], main = 'BIC', ylab = 'bic',type='l')
  lambda_mse=result[which.min(result[,2]),1]
  lambda_aic=result[which.min(result[,3]),1]
  lambda_bic=result[which.min(result[,4]),1]
  return(list(result=result, lambda_mse=lambda_mse, 
              lambda_aic=lambda_aic, lambda_bic=lambda_bic))
}

##data simulation 
set.seed(6646)
N=500;p=4
#sigmamatrix
rho=0.2
sigma_mat=diag(1,p)
for (i in 1:p)
{
  for (j in 1:p)
    sigma_mat[i,j]=rho^(abs(i-j))
}
X=rmvnorm(n = N,mean = rep(0,p),sigma = sigma_mat)
y=2*X[,1]+sqrt(6)*X[,2]+3*X[,3]-0.6*X[,4]+rnorm(n=N,mean=0,sd=1)


result = LambdaChoosing(seq(0,1,0.02),y,X,fun=ridgeRegression,fold=10,randomseed = 6543)
result[-1]

