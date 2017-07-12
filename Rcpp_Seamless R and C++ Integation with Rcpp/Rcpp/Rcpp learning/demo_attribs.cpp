#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
Numeric Vector attribs()
{
  NumericVector out=NumericVector::create(1,2,3);
  out.names()=CharacterVector::create("a","b","c");
  out.attr("my.attr")="my-value";
  out.attr("class")="my-class";
  return out;
}




