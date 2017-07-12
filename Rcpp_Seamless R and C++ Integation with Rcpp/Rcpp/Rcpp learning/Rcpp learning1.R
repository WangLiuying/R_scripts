#Rcpp learning (Seamless R and C++ integration)
#author: Wang Liuying
########################################################


########
##引入##
########


#理由：在处理数据问题上的强大
#R语言内部：核心解释器和拓展机制：C语言编写
# C++=C + 面向对象的C++ + C++模版 + STL标准库
#The Fibonacci sequence Fn
# Fn = Fn−1+Fn−2 
# F0 =0 and F1 = 1
# 0,1,1,2,3,5,8,13,21,34.

fibR <- function(n)
{
  if(n==0) return(0)
  if(n==1) return(1)
  else return(fibR(n-1)+fibR(n-2))
}
x <- 1:10
sapply(x,FUN = fibR)
#key disadvantage:inefficient
#计算时间 指数型增长
#R中的函数调用是非轻量级的。

#斐波那契数列计算的C版本
Ccode <- '
int fibonacci(const int x) {
  if(x==0) return(0);
  if(x==1) return(1);
  return( fibonacci(x-1)+fibonacci(x-2));
}'


# 如何将R中的object传给C或C++函数？
# ?.Call
# ?inline::cxxfunction

# In order to call it from R, we need to use a wrapper function
# as R prescribes a very particular interface via its .Call() function:
# all variables used at the interface have to be of pointer to S expression type,
# or SEXP.
# 为了能在R中调用C函数，我们需要使用一个封装器函数，R通过.Call函数（首选）对其接口做了特别
# 规定：所有在接口中使用的变量必须是指向S表达式的指针。
# 
# 所谓“S-表达式/运算式”或“sexp”（其中“S”代表“符号的”），是指一种以人类可读的文本形式表达半结构化数据的约定。
# S-表达式可能以其在Lisp家族的编程语言中的使用而为人所知。

# # include<Rcpp.h>
# # extern "C" SEXP fibWrapper(SEXP xs) {
# #   int x = Rcpp::as<int>(xs);
# #   int fib = fibonacci(x);
# #   return (Rcpp::wrap(fib));
# # }
# as将输入参数由SEXP转化为整型
# wrap将整型变量包装成SEXP

library(inline)
fibRcpp <- cxxfunction(signature(xs="int"),
                       plugin="Rcpp",
                       includes = Ccode,
                       body='
                       int x=Rcpp::as<int>(xs);
                       return Rcpp::wrap(fibonacci(x));',verbose=T)


# Rcpp新版本：Rcpp attributes
# 使用该框架可以自动完成变量的类型转换和编排：
# #include <Rcpp.h>
# using namespace Rcpp;
# 
# //[[Rcpp::export]]
# int fibonacci(const int x) {
#   if(x==0) return(0);
#   if(x==1) return(1);
#   return( fibonacci(x-1)+fibonacci(x-2));
# }

# Rcpp组件的核心：sourceCpp,cppFunction
cppFunction(Ccode,verbose = T,rebuild=T)


#################################
# 其它解决方案
## memoization solution courtesy of Pat Burns
mfibR <- local({
  memo <- c(1, 1, rep(NA, 1000))
  f <- function(x) {
      if (x == 0)
        return(0)
      if (x < 0)
        return(NA)
      if (x > length(memo))
        stop("x too big for implementation")
      if (!is.na(memo[x]))
        return(memo[x])
      ans <- f(x - 2) + f(x - 1)
      memo[x] <<- ans  #通过环境变量实现递归函数中的信息记录
      ans
  }
})
mfibR(100)
?local

## linear / iterative solution
fibRiter <- function(n) {
  first <- 0
  second <- 1
  third <- 0
  for (i in seq_len(n)) {
    third <- first + second
    first <- second
    second <- third
  }
  return(first)
}

#############################################################
# 性能比较
#fibR 编译成binary的fibR rcpp版
fibR.comp <- compiler::cmpfun(fibR)
fibcode <- '
int fibonacci(const int x) {
  if(x==0) return(0);
  if(x==1) return(1);
  return( fibonacci(x-1)+fibonacci(x-2));
}'
cppFunction(fibcode)
library(rbenchmark)
benchmark(fibR(20),fibR.comp(20),fibonacci(20))




##########################################
##############
# 工具与设置 #
##############

# 1.编译器配置
# install.packages("inline")
# install.packages("rbenchmark")
# install.packages("RUnit")

####################################
# 2.R应用程序接口API
?.C
?.Call
# .Call() interface exclusively. It can operate on the so-called SEXP objects, 
# which stands for pointers to S expression objects.
# .C只支持指向C中基本类型的指针
# .Call可以操作SEXP对象，即指向S表达式的指针对象

# 手动编译方法：
# 假设你有了一个cpp文件
# 第一步，设置环境变量说明头文件和库文件位置
# 第二步，使用编译器g++进行编译(*.o文件)并将其链接为一个共享库(*.so文件)，so文件可供我们载入R
# 接着，我们就可以加载共享库中的目标文件，在R中使用.Call调用

# 听起来有点复杂，别着急，后面讨论的inline和Rcpp attri使这个过程变得非常简单
########################################

##inline包
#例子：卷积计算
library(inline)
?cxxfunction
src <- '
  Rcpp::NumericVector xa(a);
  Rcpp::NumericVector xb(b);
  int n_xa=xa.size(), n_xb=xb.size();
  Rcpp::NumericVector xab(n_xa+n_xb-1);
  for(int i=0;i<n_xa;i++)
    for(int j=0;j<n_xb;j++)
      xab[i+j]+=xa[i]*xb[j];
  return xab;
'
myfun <- cxxfunction(signature(a="numeric",b="numeric"),body=src,plugin="Rcpp",
                     verbose=T)
#rcpp() 默认plugin="Rcpp"的cxxfunction
myfun(1:4,2:5)
# 参数说明：signature函数头（输入变量）；body函数主体；plugin包含库；
# verbose=T输出生成的cpp文件和编译、链接调用过程细节；
# includes包含额外的代码块
# 
# 另外，cxxfunciton中使用随机产生的函数名和BEGIN_RCPP&END_RCPP两个宏




#使用includes
inc <- '
  template<typename T>
  class square : public std::unary_function<T,T>
{
  public:
    T operator()(T t) const {return t*t;}
  };
'
src <- '
  double x=Rcpp::as<double>(xs);
  int i=Rcpp::as<int>(is);
  square<double> sqdbl;
  square<int> sqint;
  Rcpp::DataFrame df=
    Rcpp::DataFrame::create(Rcpp::Named("x",sqdbl(x)),
                            Rcpp::Named("i",sqint(i)));
  return df;
'
fun <- cxxfunction(sig = signature(xs="numeric",is="integer"),
                   body=src,includes = inc,plugin = "Rcpp")
fun(2.2,3L)

# #模板类
# template<class 模板参数表>
# 
#   class 类名｛
# 
# // 类定义．．．．．．
# 
# ｝；

#使用plugin
#这里用到了Rcpp以外的库(RcppArmadillo，整合了C++的Armadillo)
#这个例子重写了线性回归lm()
src <- '
  Rcpp::NumericVector yr(ys);
  Rcpp::NumericMatrix Xr(Xs);
  int n=Xr.nrow(),k=Xr.ncol();
  arma::mat X(Xr.begin(),n,k,false);
  arma::colvec y(yr.begin(),yr.size(),false);
  arma::colvec coef=arma::solve(X,y);// fit y~X
  arma::colvec res=y-X*coef;//residuals
  double s2=std::inner_product(res.begin(),res.end(),res.begin(),double())/(n-k);
  arma::colvec se=arma::sqrt(s2*
                  arma::diagvec(arma::inv(arma::trans(X)*X)));
  return Rcpp::List::create(Rcpp::Named("coef")=coef,
                            Rcpp::Named("se")=se,
                            Rcpp::Named("df")=n-k);
'
fun <- cxxfunction(sig=signature(ys="numeric",Xs="numeric"),
                   body=src,plugin="RcppArmadillo",verbose = T)
#当然你也可以自己修改或制作用于plugin的库文件

#######################################################
##Rcpp attribute
#sourceCpp
#cppFunction
#evalCpp

cpptxt <- '
  int fibonacci(const int x)
{
  if(x<2) return(x);
  return(fibonacci(x-1)+fibonacci(x-2));
}
'
cppFunction(cpptxt)
fibCpp <- cppFunction(cpptxt,verbose = T)
#verbose输出：组织cpp临时文件，生成的so文件，链接装载过程
?cppFunction

#Rcpp组件简洁好用，而inline组件更加成熟

############################################
##异常处理

?tryCatch
?try
rm(a)
a <- log("222")
a
a <- try(log("222"))
a
page(try)
e <- simpleError("test error")
w <- simpleWarning("warning test")
tryCatch(1+1,error=function(e) 3+3,finally=print("Hello"))
tryCatch(stop(e),error=function(e) 3+3,warning=function(e) 3*3,finally=print("Hello"))
tryCatch(warning(w),error=function(e) 3+3,warning=function(e) 3*3,finally=print("Hello"))
##not run
# extern "C" SEXP fun(SEXP x)
# {
#   try{
#     int dx=Rcpp::as<int>(x);
#     if (dx>10)
#       throw std::range_error("too big");
#     return Rcpp::wrap(dx*dx);
#   }
#   catch(std::exception$ __ex__){
#     forward_exception& __ex__
#   }
#   catch(...){
#     ::Rf_error("c++ exception(unknown reason)");
#   }
#   return R_NilValue; //not reached
# }
# cxxfunction 中的BEGIN_RCPP & END_RCPP -构成异常机制的宏
fun <- cxxfunction(sig=signature(ys="numeric",Xs="numeric"),
                   body=src,plugin="RcppArmadillo",verbose = T)
fun("a",3)


###################################################################################

