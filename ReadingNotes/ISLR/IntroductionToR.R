### vectors, data, matrices, subsetting
x=c(2,7,5)
x
y=seq(from=4,length=3,by=3)
?seq
y
x+y
x/y
x^y
x[2]
x[2:3]
x[-2]
x[-c(1,2)]
z=matrix(seq(1,12),4,3)
z
z[3:4,2:3]
z[,2:3]
z[,1]
z[,1,drop=FALSE]
dim(z)
ls()##is equal to `objects()` function. It will return a vector of character strings 
##giving the names of the objects in the specified environment.
rm(y)
ls()
### Generating random data, graphics
x=runif(50)## uniform distribution
y=rnorm(50)## nomal distribution
plot(x,y)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
par(mfrow=c(2,1))##par can be used to set or query graphical parameters.
plot(x,y)
hist(y)
par(mfrow=c(1,1))
### Reading in data
Auto=read.csv("Auto.csv")
##?pwd()
Auto=read.csv("D:/data analysis/R's workingspace/Auto.csv")
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)
search()
attach(Auto)##让Auto中的变量进入global环境，取消用`detach()`函数
search()
plot(cylinders,mpg)
cylinders=as.factor(cylinders)
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
pdf(file="D:/data analysis/R's workingspace/mpg.pdf")##打开开关，使图像device开始输出pdf
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")##这个图像会被输出
dev.off()##关闭pdf输出
pairs(Auto,col="brown")## a matrix of scatterplots
pairs(mpg~cylinders+acceleration+weight,Auto)
q()
