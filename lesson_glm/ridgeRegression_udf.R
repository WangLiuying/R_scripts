

##packages loading
library(mvtnorm)





#ridge regression
#input: y, X, lambda; output: coef, predictfunction

ridgeRegression = function(y, X, lambda)
{
  beta_ridge = solve(t(X) %*% X + lambda * diag(1, dim(X)[2])) %*% t(X) %*% y
  ridgePredict = function (X){return (X %*% beta_ridge)}
  result = list(lambda = lambda, coef_ridge = beta_ridge, ridgePredict = ridgePredict)
  return(result)
}
result=ridgeRegression(y, X, 0.01)


#cross validation 
#input: y, X, lambda, fold, criterion, randomseed; output: criterion value, figure, max point


computeOneFold = function(reindex, y_ridge = y_ridge, X_ridge = X_ridge, lambda = lambda)
{
  fitmodel = ridgeRegression(y = y_ridge[-reindex], X = X_ridge[-reindex,], lambda=lambda)
  y_pre = fitmodel$ridgePredict(X = X_ridge[reindex,])
  rss = sum((y_ridge[reindex] - y_pre)^2)
  return(rss)
}

ridgeCV = function(lambda, y_ridge, X_ridge, fold = fold, randomseed = randomseed)
{
  #divide into folds
  if(!is.null(randomseed)) set.seed(randomseed)
  reindex=sample(1:length(y_ridge), size=length(y_ridge), replace = F)
  foldsize = length(y_ridge) %/% fold
  folddivide = list()
  for (f in 1:fold)
  {
    folddivide = c(folddivide, list(reindex[((f-1)*foldsize+1):(f*foldsize)]))
  }
  rss = sum(sapply(folddivide,FUN = computeOneFold, y_ridge=y_ridge, X_ridge = X_ridge, lambda=lambda))
  N = length(y_ridge)
  eigenvalue = eigen(t(X_ridge)%*%X_ridge)$values
  df =  sum(eigenvalue / (eigenvalue + lambda))
  mse = rss / N
  aic = N * log(rss / N) + 2 * df
  bic = N * log(rss / N) + log(N) * df
  return(c(lambda = lambda, MSE = mse, AIC = aic, BIC = bic))
}

ridgeLambdaChoosing = function(lambdaseq, y_ridge, X_ridge, fold = 5, randomseed = NULL)
{
  result = t(sapply(lambdaseq, FUN = ridgeCV,
                    y_ridge = y_ridge, X_ridge = X_ridge, fold = fold, randomseed = randomseed))
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


ridgeLambdaChoosing(seq(0,1,0.005),y,X,fold=10)
finalmodel = ridgeRegression(y, X, lambda=0.001)
finalmodel$coef_ridge
#second data
N = 500; p = 4
set.seed(6648)
sigma_mat = matrix(0.2, ncol = p, nrow = p)
for (i in 1:p){sigma_mat[i,i] = 1}
X=rmvnorm(n = N, mean = rep(0, p), sigma = sigma_mat)
X[,2] = 0.75 * X[,3] + 0.5 * X[,4] + rnorm(n = N, mean = 0, sd = 1)
y = 1.5 * X[,1] +2 * X[,2] - 0.7 * X[,3] +0.9 * X[,4] + rnorm(n = N, mean = 0, sd = 1)

ridgeLambdaChoosing(seq(0,2,0.01), y, X, fold = 10, randomseed = 6645)
finalmodel = ridgeRegression(y, X, 1.13)
finalmodel$coef_ridge
