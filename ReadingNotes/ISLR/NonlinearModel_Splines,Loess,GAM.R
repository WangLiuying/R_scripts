#非线性建模

library(ISLR)
attach(Wage)

#多项式回归和阶梯函数
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
#核心函数：poly
fit=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit))


agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])

#多项式+ANOVA
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)


##多项式logistic
fit=glm(I(wage>250)~poly(age,4),data=Wage,family = "binomial")
preds=predict(fit,newdata=list(age=age.grid),type="response")
preds

############################################################

#样条

library(splines)

fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
pred
plot(age,wage,col="grey")
lines(age.grid,pred$fit,lwd=2)

#样条选择
bs(age,knots=c(25,40,60))
dim(bs(age,knots=c(25,40,60)))
bs(age,df=6)
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")#设定自由度为6时自动选择的结点

#自然样条

fit=lm(wage~ns(age,df=4),data=Wage)
pred=predict(fit,newdata=list(age=age.grid))
lines(age.grid,pred,col="red",lwd=2)

#光滑样条
fit=smooth.spline(age,wage,df=16)
fit
lines(fit,col="blue",lwd=2)
fit2=smooth.spline(age,wage,cv=T)
fit2$df
lines(fit2,col="purple",lwd=2)


############################################
#局部回归

fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)


##############################################
#GAM
gam1 <- lm(wage~ns(age,5)+education,data=Wage)
library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=T,col="blue")
plot.gam(gam.m3,se=T,col="red")

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

summary(gam.m2)

preds=predict(gam.m2,newdata=Wage)

gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)

plot(gam.lo,se=T,col="green")


gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)

library(akima)#可视化
plot(gam.lo.i)

############################################################
#logistic GAM

gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

table(education,I(wage>250))
#有问题

gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,
             subset=(education !="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
