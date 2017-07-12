##linear regression
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)
lm.fit <- lm(medv~lstat,data=Boston)


lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")

with(data=Boston,{plot(lstat,medv);abline(lm.fit)})

attach(Boston)
par(mfrow=c(2,2))
plot(lm.fit)


plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

##multiple linear regression

lm.fit <- lm(medv~lstat+age,data=Boston)
summary(lm.fit)


library(car)
lm.fit <- lm(medv~.,data=Boston)
vif(lm.fit)
lm.fit1 <- update(lm.fit,medv~.-age)


##interaction
summary(lm(medv~lstat*age,data=Boston))

##预测变量的非线性变换
lm.fit <- lm(medv~lstat,data=Boston)
lm.fit2 <- lm(medv~lstat+I(lstat*2),data=Boston)
summary(lm.fit2)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)


#多项式拟合
attach(Boston)
lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5)
detach(Boston)

##定性预测变量
fix(Carseats)
names(Carseats)
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc) #查看虚拟变量的编码


