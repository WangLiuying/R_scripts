library(MASS)
library(ISLR)
### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1 # a brief report with only coefficients
summary(fit1)# a report with more details
abline(fit1,col="red") #add a new line(fitted line) which is described by `fit1`
names(fit1)
confint(fit1) #derive the confident interval of coefficients
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence") #make prediction with given predictors
### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
fit4=update(fit3,~.-age-indus)
summary(fit4)
### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)#with `*` notation main effects & interactions will be included
summary(fit5)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",type="l",pch=20)
fit7=lm(medv~poly(lstat,4))#by using `poly` function we can easily fit a polynomial
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)
###Qualitative predictors
fix(Carseats)# `fix` function allows us open a new window for editing data in x
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)#the `:` notation also represents interaction.
summary(fit1)
contrasts(Carseats$ShelveLoc)# we can see how R deal with qualitative predictors beneath.
###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...)#we add `...` to allow extra parameters getting into function `plot`
  {
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)



