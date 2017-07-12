#Rcpp learning (Seamless R and C++ integration)
#author: Wang Liuying
#data structure
####################################################

#RObject 类
# 每个RObject实例都封装了一个R对象，每个R对象在其内部可表达为一个SEXP（指向S表达式的指针）
# 所谓“S-表达式/运算式”或“sexp”（其中“S”代表“符号的”），是指一种以人类可读的文本形式表达半结构化数据的约定。
# S-表达式可能以其在Lisp家族的编程语言中的使用而为人所知。
# S表达式是不透明的，需用辅助函数访问。
# RCPP为用户自动管理内存分配和释放，RObject类是重要一环，它封装了SEXP类型（SEXP是RObject的唯一成员）。
# 
# 用户可见的类：
# IntegerVector：integer类型向量
# NumericVector：numeric类型向量
# LogicalVector：logical类型向量
# CharacterVetor：character类型向量
# GenericVector：list类型的泛型向量
# ExpressionVector：expression类型的向量
# 
# 其它IntegerMatrix、NumericMatrix


# IntegerVector：integer类型向量
# 提供了从标准的R整型向量及到标准的R整形向量的自然映射。
# 模版函数as<>()和wrap()
# 
# ################################
# IntegerVector：integer类型向量#
#################################
# 简单例子：输出完美数6、28、496、8182
library(inline)
src <- '
  Rcpp::IntegerVector epn(4);
  epn[0]=6;
  epn[1]=14;
  epn[2]=496;
  epn[3]=8182;
  return epn;
'
fun <- cxxfunction(signature(),src,plugin = "Rcpp")
fun()
#hint:创建一个向量；允许逐一赋值元素；隐式调用wrap

#简单例子：prod连乘
src <- "Rcpp::IntegerVector vec(vx);
int prod=1;
for(int i=0;i<vec.size();i++)
{
  prod*=vec[i];
}
return Rcpp::wrap(prod);"
fun <- cxxfunction(signature(vx="integer"),src,plugin = "Rcpp")
fun(1:10)
#hint
# 隐式使用as<>;仅拷贝指针而不拷贝数据

src <- '
Rcpp::IntegerVector vec(vx);
int prod=std::accumulate(vec.begin(),vec.end(),1,std::multiplies<int>());
return Rcpp::wrap(prod);
'
fun <- cxxfunction(signature(vx="integer"),src,plugin = "Rcpp")
fun(1:10)
#hint：使用了标准模版库STL-std中的accumulate函数和模版函数multiplies

#自动的类型转换
fun(1:10)
fun(seq(1.0,1.9,by=0.1))#float->int 截断

fun(LETTERS[1:10])#Error

##################
#NumbericVector类#
##################
# Rcpp中最常用的类，对应R中最基本的数值向量，存储类型double

src <- '
Rcpp::NumericVector vec(vx);
double p=Rcpp::as<double>(dd);
double sum=0.0;
  for (int i=0;i<vec.size();i++)
  {
    sum+=pow(vec[i],p);
  }
return(Rcpp::wrap(sum));
'
fun <- cxxfunction(signature(vx="numeric",dd="numeric"),src,plugin = "Rcpp")
x <- 1:4
fun(x,dd=2.2);x

#######################################
#clone的使用
src <- '
Rcpp::NumericVector invec(vx);

for (int i=0;i<invec.size();i++)
  {
    invec[i]=log(invec[i]);
  }
return invec;
'

fun <- cxxfunction(signature(vx="numeric"),src,plugin="Rcpp")
x <- seq(1.0,3.0,by=1)
cbind(x,fun(x))
# hint:输入输出同一个指针（sexp），直接在sexp上进行操作，使得参数传入的R对象也被更改了

src <- '
Rcpp::NumericVector invec(vx);
Rcpp::NumericVector outvec=Rcpp::clone(vx);
for (int i=0;i<invec.size();i++)
  {
    outvec[i]=log(invec[i]);
  }
return outvec;
'
fun <- cxxfunction(signature(vx="numeric"),src,plugin="Rcpp",verbose = T)
x <- seq(1.0,3.0,by=1)
cbind(x,fun(x))
#hint:wrap 和as 都是可以隐式使用的，故不需要进行封装。直接返回outvec。

########
#Matrix#
########
# 三维数组示例
# Rcpp::NumericVector vec3=
   # Rcpp::NumericVector(Rcpp::Dimension(4,5,6));

src <- '
Rcpp::NumericMatrix mat=
  Rcpp::clone<Rcpp::NumericMatrix>(mx);
std::transform(mat.begin(),mat.end(),mat.begin(),::sqrt);
return mat;
'
fun <- cxxfunction(signature(mx="numeric"),src,plugin = "Rcpp")
orig <- matrix(1:9,3,3)
fun(orig)

src <- '
Rcpp::NumericMatrix mat=
Rcpp::clone<NumericMatrix>(mx);
std::transform(mat.begin(),mat.end(),mat.begin(),::sqrt);
return mat;
'
fun <- cxxfunction(signature(mx="numeric"),src,plugin = "Rcpp")
orig <- matrix(seq(1.1,9.9,by=1.1),3,3)
fun(orig)

############
#其它向量类#
############

#LogicalVector

fun <- cxxfunction(signature(),plugin = "Rcpp",
                   body='
                   Rcpp::LogicalVector v(6);
                   v[0]=v[1]=false;
                   v[2]=true;
                   v[3]=R_NaN;
                   v[4]=R_PosInf;
                   v[5]=NA_REAL;
                   return v;')
fun()

#CharacterVector

fun <- cxxfunction(signature(),plugin = "Rcpp",
                   body='
                   Rcpp::CharacterVector v(3);
                   v[0]="The quick brown";
                   v[1]="fox";
                   v[2]=R_NaString;
                   return v;')
fun()

#RawVector 可用来处理字节数据

#######################################################

#Named类
# 辅助类，用于设定键值对中的键，例如
someVec <- c(mean=1.23,dim=42.0,cnt=12)

src <- '
Rcpp::NumericVector x=
  Rcpp::NumericVector::create(
Rcpp::Named("mean")=1.23,
Rcpp::Named("dim")=42,
Rcpp::Named("cnt")=12
);
return x;
'
fun <- cxxfunction(signature(),src,plugin="Rcpp")
fun()
# 可应用缩写形式：
src <- '
Rcpp::NumericVector x=
  Rcpp::NumericVector::create(
_["mean"]=1.23,
_["dim"]=42,
_["cnt"]=12
);
return x;
'
fun <- cxxfunction(signature(),src,plugin="Rcpp")
fun()


#List类（GenericVector类）
#这里不是完整的代码，勿运行

# 由R向C传入一个list，取出list中的值并转为相应类型的数据

# RcppExport SEXP DEoptim(SEXP lowerS, SEXP upperS,
#                         2 SEXP fnS, SEXP controlS, SEXP rhoS) {
#   Rcpp::NumericVector f_lower(lowerS), f_upper(upperS);
#   Rcpp::List control(controlS);
#   double VTR = Rcpp::as<double>(control["VTR"]);
#   int i_strategy = Rcpp::as<int>(control["strategy"]);
#   int i_itermax = Rcpp::as<int>(control["itermax"]);
#   int i_D = Rcpp::as<int>(control["npar"]);
#   int i_NP = Rcpp::as<int>(control["NP"]);
#   int i_storepopfrom = Rcpp::as<int>(control["storepopfrom"])-1;
#   int i_storepopfreq = Rcpp::as<int>(control["storepopfreq"]);
#   int i_specinitialpop = Rcpp::as<int>(control["specinitialpop"]);
#   Rcpp::NumericMatrix initialpopm =
#     Rcpp::as<Rcpp::NumericMatrix>(control["initialpop"]);
#   double f_weight = Rcpp::as<double>(control["F"]);
#   double f_cross = Rcpp::as<double>(control["CR"]);
#   [...]
# }

#由C++返回一个list

# return Rcpp::List::create(Rcpp::Named("bestmem") = t_bestP,
#                           Rcpp::Named("bestval") = t_bestC,
#                           Rcpp::Named("nfeval") = l_nfeval,
#                           Rcpp::Named("iter") = i_iter,
#                           Rcpp::Named("bestmemit") =
#                             t(d_bestmemit),
#                           Rcpp::Named("bestvalit") = d_bestvalit,
#                           Rcpp::Named("pop") = t(d_pop),
#                           Rcpp::Named("storepop") = d_storepop);)

#或：
# Rcpp::Listll(4);ll[1]= 预留四位但不能超出范围
#push_back() push_front 低效

#DataFrame类

src <- '
Rcpp::IntegerVector v=
  Rcpp::IntegerVector::create(7,8,9);
std::vector<std::string> s(3);
s[0]="x";
s[1]="y";
s[2]="z";
return Rcpp::DataFrame::create(Rcpp::Named("a")=v,Rcpp::Named("b")=s);'

fun <- cxxfunction(signature(),src,plugin="Rcpp")
fun()

#Function类
#第一种方法：把function当作一个参数传入C++
src <- '
Function sort(x);
return sort(y,Named("decreasing",true));
'
fun <- cxxfunction(signature(x="function",y="ANY"),src,plugin = "Rcpp")
fun(sort,1:10)
fun(order,3:9)
fun(sort,LETTERS[1:10])
#第二种方法：访问R函数

src <- '
RNGScope scp;
Rcpp::Function rt("rt");
return rt(5,3);
'
fun <- cxxfunction(signature(),src,plugin="Rcpp")
set.seed(42)
fun()
set.seed(42)
rt(5,3)


src <- '
Rcpp::Function prod("prod");
return wrap(prod(y));
'
fun <- cxxfunction(signature(y="numeric"),src,plugin="Rcpp")
fun(1:10)



