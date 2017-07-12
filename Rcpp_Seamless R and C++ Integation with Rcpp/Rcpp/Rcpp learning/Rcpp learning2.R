# Rcpp learning (advance R)
# author: WangLiuying
setwd("C:/Users/River/Desktop/2016研一下/club课程")
#循环的向量化
#递归
#使用C++中的高级数据结构和算法

#最重要的函数：
# cppFunction()
# sourceCpp()

# 在Windows 上，安装Rtools(http://cran.r-project.org/bin/windows/Rtools/)
# 在Mac 上，从app store 上安装Xcode
# 在Linux 上，sudo apt-get install r-base-dev 或者类似的命令

library(Rcpp)
# int add(int x,int y,int z){
#   int sum=x+y+z;
#   return sum;
# }
cppFunction('
int add(int x,int y,int z){
  int sum=x+y+z;
  return sum;
}')
add
add(1,2,3)

# 当你运行这段代码时，Rcpp 将编译C++代码，并且构造一个R 函数，该R 函数会
# 连接到编译的C++函数。


#简单C++
#1 none to scaler
one <- function() 1L

# int one(){
#   return 1;
# }

#numeric=double,integer=int,character=String,logical=bool
#NumericVector,IntegerVector,CharacterVector,LogicalVector
#显式返回；分号结束

#2 scaler to scaler 
signR <- function(x){
  if(x>0){1}
  else if(x==0){0}
  else{-1}
}

# int signC(int x){
#   if(x>0){return 1;}
#   else if(x==0) {return 0;}
#   else {return -1;}
# }

#3 vector to scaler
sumR <- function(x){
  total <- 0
  for(i in seq_along(x))
  {total <- total+x[i]}
  total   
}


cppFunction("double sumC(NumericVector x)
{
  int n=x.size();
  double total=0;
  for(int i=0;i<n;++i)
  {total += x[i];}
  return total;
}")

#install.packages("microbenchmark")
library(microbenchmark)
x <- runif(1e3)
length(x)
microbenchmark(sum(x),sumC(x),sumR(x))

#4 vector to vector
pdistR <- function(x,ys)
{
  sqrt((x-ys)^2)
}

cppFunction(
"NumericVector pdistC(double x,NumericVector ys)
{
  int n=ys.size();
  NumericVector out(n);
  for(int i=0;i<n;++i)
  {out[i]=sqrt(pow(ys[i]-x,2.0));}
  return out;
}")

#5 matrix to vector
#矩阵类型：NumericMatrix IntegerMatrix CharacterMatrix LogicalMatrix

cppFunction(
"NumericVector rowSumsC(NumericMatrix x)
{
  int nrow=x.nrow(),ncol=x.ncol();
  NumericVector out(nrow);
  for(int i=0;i<nrow;i++)
  {
    double total=0;
    for(int j=0;j<ncol;j++)
    {total+=x(i,j);}
    out[i]=total;
  }
  return out;
}")
#矩阵索引用()
set.seed(1014)
x <- matrix(sample(100),10)
rowSums(x)
rowSumsC(x)


###################################
#使用cppSource()
#文本编辑器Notepad+
#需要输出的函数使用 //[[Rcpp:export]]
library(Rcpp)
sourceCpp("rcpptest.cpp")

sourceCpp("rcpp_identify.cpp")
x=1:9
mean(x);f1(x)
microbenchmark(mean(x),f1(x))


cumsum(x);f2(x)
microbenchmark(cumsum(x),f2(x))

x=c(F,F,F,F,T,F)
f3(x);any(x)

which.max(c(1,2,3,4,5)>3)
f4(function(x) x>3,c(1,2,3,4,5))


x <- c(1,3,2,4,5);y <- c(2,5,3)
f5(x,y);pmin(x,y)

sourceCpp("rcpp_practice.cpp")

##demo:S3 class
sourceCpp("demo_mpe.cpp")
x <- 1:100;y <- 41:140+rnorm(100)
lmdemo <- lm(y~x)
mpe(lmdemo)


