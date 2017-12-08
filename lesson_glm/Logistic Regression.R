##Logistic Regression Algorithm
library(mvtnorm)


##testdata
beta = c(1,2,0,0,3,0,0,0,-2,0)
alpha = 1
rho = 0.5
N = 2000
sigmaMatrix = matrix(0,10,10)
for (i in 1:dim(sigmaMatrix)[1])
{
  for (j in 1:dim(sigmaMatrix)[2])
  {
    sigmaMatrix[i,j] = rho^abs(i-j)
  }
}
X = rmvnorm(n=N, sigma = sigmaMatrix) 
prob = exp(alpha + X%*%beta)/(1+exp(alpha + X%*%beta))
y = c()
for (i in 1:N)
{
  y[i] = rbinom(n = 1, size = 1, prob = prob[i])
}
X = cbind(rep(1,N),X)
beta = c(alpha,beta)


##需要用到的中间计算

prob_logit = function(X, beta)
{
  1 - 1/(1+exp(X%*%beta))
}

#LogLikelihood: X 设计矩阵，需要自行加入常数列 beta含beta0
LogLikelihood_logistic = function(X,y,beta)
{
  sum(y*(X%*%beta)-log(1+exp(X%*%beta)))
}

#first derivative wrt beta_j for Logistic(no penalty)
djLogll_logistic = function(X,y,beta,j)
{
  sum(X[,j]*(y-prob_logit(X,beta)))
}
#second derivative wrt beta_j for Logistic(no penalty)
ddjLogll_logistic = function(X,y,beta,j)
{
  -sum(X[,j]^2*prob_logit(X,beta)*(1-prob_logit(X,beta)))
}
#Coordinal descent
GSupdate = function (X, y, initBeta = NULL, tol = 1e-6, printDetail = FALSE)
{
  num_beta = dim(X)[2] #beta维度
  if(is.null(initBeta)) {initBeta = rep(0,num_beta)} #初始化Beta
  beta_new = initBeta;  beta_old = rep(9,num_beta)
  stopCheck = function(X,y,beta_new,beta_old) #停止条件：Loglikelihood
  {
    LogNew = LogLikelihood_logistic(X,y,beta_new)
    LogOld = LogLikelihood_logistic(X,y,beta_old)
    Logdiff = abs(LogNew - LogOld)/abs(LogOld)
    return(list(Logdiff = Logdiff, LogNew = LogNew))
  } 
  likelihood= stopCheck(X,y,beta_new,beta_old)
  i = 0
  while(likelihood[[1]] > 1e-6) #检查停止条件，不满足则继续迭代
  {
    i = i+1
    beta_old = beta_new #储存上一轮结果
    for (j in 1:num_beta) #逐一更新beta_j
    {
      beta_new[j] = beta_old[j] - 1/(ddjLogll_logistic(X,y,beta_new,j)+1e-6)*djLogll_logistic(X,y,beta_new,j)
    }
    likelihood = stopCheck(X,y,beta_new,beta_old)#计算更新likelihood效果
    if (printDetail) #打印每次迭代的结果
      {cat ('iterRound:', i, ' Loglikehood:', likelihood[[2]], '\n')}
  }
  return(list(beta = beta_new, loglikelihood = likelihood[[2]]))
}

GSupdate(X,y,printDetail = T) ##测试一下：正常运行


###############################################
##SCAD
####################################################
penalty = function(theta,lambda,a)
{
  if(theta < lambda) return(theta * lambda)
  else if(theta > lambda*a) return(lambda^2*(a^2-1)/(2*a - 2)) 
  else return((a*lambda*theta - 0.5*(theta^2+lambda^2))/(a-1))
}

dpenalty = function (theta,lambda,a) 
{
  lambda * (as.numeric(theta<=lambda)
             + max(a*lambda-theta,0)/((a-1)*lambda)*as.numeric(theta > lambda))
}

Loglikelihood_scad = function(X,y,beta,lambda,a)
{
  +sum(y*(X%*%beta)-log(1+exp(X%*%beta)))- length(y)*sum(sapply(abs(beta[-1]),penalty,lambda,a))
}

djLogll_scad = function(X,y,beta,j,lambda,a)
{
  +sum(X[,j]*(y-prob_logit(X,beta)))- as.numeric(j!=1)*length(y)*dpenalty(abs(beta[j]),lambda,a)/abs(beta[j])*beta[j]
}

ddjLogll_scad = function(X,y,beta,j,lambda,a)
{
  -sum(X[,j]^2*prob_logit(X,beta)*(1-prob_logit(X,beta))) - as.numeric(j!=1)* length(y)*dpenalty(abs(beta[j]),lambda,a)/abs(beta[j])
}


GSupdate_scad = function (X, y, lambda, a, initBeta = NULL, tol = 1e-6, printDetail = FALSE)
{
  num_beta = dim(X)[2] #beta维度
  if(is.null(initBeta)) {initBeta = GSupdate(X,y,tol = tol)[[1]]} #初始化Beta
  beta_new = initBeta;  beta_old = initBeta + 2
  stopCheck = function(X,y,beta_new,beta_old,lambda,a) #停止条件：Loglikelihood
  {
    LogNew = Loglikelihood_scad(X,y,beta_new,lambda,a)
    LogOld = Loglikelihood_scad(X,y,beta_old,lambda,a)
    Logdiff = abs(LogNew - LogOld)/abs(LogOld)
    return(list(Logdiff = Logdiff, LogNew = LogNew))
  } 
  likelihood = stopCheck(X,y,beta_new,beta_old,lambda, a)
  i = 0
  while(likelihood[[1]] > 1e-6) #检查停止条件，不满足则继续迭代
  {
    i = i+1
    beta_old = beta_new #储存上一轮结果
    for (j in 1:num_beta) #逐一更新beta_j
    {
      if (beta_new[j] == 0 & j != 1) next;
      beta_new[j] = beta_old[j] - 1/(ddjLogll_scad(X,y,beta_new,j,lambda,a))*djLogll_scad(X,y,beta_new,j,lambda,a)
      if (beta_new[j] < tol & j != 1) beta_new[j] = 0
    }
    likelihood = stopCheck(X,y,beta_new,beta_old,lambda,a)#计算更新likelihood效果
    if (printDetail) #打印每次迭代的结果
    {cat ('iterRound:', i, ' Loglikehood:', likelihood[[2]],'\n',beta_new, '\n')}
  }
  return(list(beta = beta_new, loglikelihood = likelihood[[2]]))
}

GSupdate_scad(X,y,2,3.7,tol = 1e-6,printDetail = T)#测试

##Package检验算法
Logistic_model = glm.fit(x = X,y = y,family = binomial(link = 'logit'))
Logistic_model$coefficients
GSupdate(X,y)[[1]]

library(ncvreg)
Logistic_scad = ncvreg(X[,-1],y,family = 'binomial', penalty = 'SCAD', gamma = 3.7, lambda = 0.1,eps = 1e-4,returnX = T)
as.vector(Logistic_scad$beta)
X_std = cbind(1,Logistic_scad$X)
GSupdate_scad(X_std,y,0.1,3.7,tol = 1e-4)
GSupdate_scad(X,y,0.1,3.7,tol = 1e-4)
