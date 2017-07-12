#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject callWithOne(Function f) {
  return f(1);
}

// [[Rcpp::export]]
List lapply1(List input, Function f) {
  int n = input.size();
  List out(n);
  for(int i = 0; i < n; i++) {
    out[i] = f(input[i]);
  }
  return out;
}