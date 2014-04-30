#ifndef _RODEINT_POINTER_HPP_
#define _RODEINT_POINTER_HPP_

#include <iostream>

struct foo {
  foo(double a_, double b_) : a(a_), b(b_) {}
  ~foo() {std::cout << "Deleting foo\n" << std::endl;}
  double run() {return a+b;}
  double a, b;
};

#include <RcppCommon.h>

namespace Rcpp {
template<>
SEXP wrap(const foo&);

template<>
foo as(SEXP);
}

#endif
