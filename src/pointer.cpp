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
  return static_cast<foo>(*xp);
}
}

// [[Rcpp::export]]
foo make_foo(double a, double b) {
  return foo(a, b);
}

// [[Rcpp::export]]
double foo_run(foo obj) {
  return obj.run();
}
