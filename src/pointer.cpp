// Quick test of some pointer stuff with Rcpp.

#include "pointer.hpp"

// In contrast with how this approach should normally be done, this
// file *only* contains interfaces.
#include <Rcpp.h>

// [[Rcpp::export]]
rodeint::foo make_foo(double a, double b) {
  return rodeint::foo(a, b);
}

// [[Rcpp::export]]
double foo_run(Rcpp::XPtr<rodeint::foo> obj) {
  return obj->run();
}

// [[Rcpp::export]]
void foo_set_a(Rcpp::XPtr<rodeint::foo> obj, double a) {
  obj->a = a;
}

// [[Rcpp::export]]
double foo_run_copy(const rodeint::foo& obj) {
  return obj.run();
}
