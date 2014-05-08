#ifndef _RODEINT_RCPP_AS_WRAP_POST_TARGET_HPP_
#define _RODEINT_RCPP_AS_WRAP_POST_TARGET_HPP_

// This organises loading all full class definitions, and the as/wrap
// prototypes
#include "rodeint_target.h"

// Now it should be safe to include Rcpp.h
#include <Rcpp.h>

namespace Rcpp {
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::target_class& obj) {
  XPtr<rodeint::target_class> ret(new rodeint::target_class(obj), true);
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::target_cpp& obj) {
  XPtr<rodeint::target_cpp> ret(new rodeint::target_cpp(obj), true);
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::target_r& obj) {
  XPtr<rodeint::target_r> ret(new rodeint::target_r(obj), true);
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
inline rodeint::target_class as(SEXP obj) {
  XPtr<rodeint::target_class> xp(obj);
  return *xp;
}
template<>
inline rodeint::target_cpp as(SEXP obj) {
  XPtr<rodeint::target_cpp> xp(obj);
  return *xp;
}
template<>
inline rodeint::target_r as(SEXP obj) {
  XPtr<rodeint::target_r> xp(obj);
  return *xp;
}
}


#endif
