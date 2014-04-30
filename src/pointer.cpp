// Quick test of some pointer stuff with Rcpp.
//
// Doing this with non-modifiable things makes the most sense -- it's
// easy enough to return a copy from the object.  That's what the as

#include <Rcpp.h>
#include "pointer.hpp"

namespace Rcpp {
template<>
SEXP wrap(const foo& obj) {
  XPtr<foo> ret(new foo(obj), true);
  return wrap(ret);
}

template<>
foo as(SEXP obj) {
  XPtr<foo> xp(obj);
  return *xp;
}
}

// [[Rcpp::export]]
foo make_foo(double a, double b) {
  return foo(a, b);
}

// [[Rcpp::export]]
double foo_run(Rcpp::XPtr<foo> obj) {
  return obj->run();
}

// [[Rcpp::export]]
void foo_set_a(Rcpp::XPtr<foo> obj, double a) {
  obj->a = a;
}

// [[Rcpp::export]]
double foo_run_copy(const foo& obj) {
  return obj.run();
}
