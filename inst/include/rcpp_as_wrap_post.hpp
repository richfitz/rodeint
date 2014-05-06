#ifndef _RODEINT_RCPP_AS_WRAP_POST_HPP_
#define _RODEINT_RCPP_AS_WRAP_POST_HPP_

// This organises loading all full class definitions, and the as/wrap
// prototypes
#include "rodeint.h"

// Now it should be safe to include Rcpp.h
#include <Rcpp.h>

// The target_r and target_cpp definitions are done elsewhere
#include "rcpp_as_wrap_post_target.hpp"

namespace Rcpp {
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::stepper& obj) {
  XPtr<rodeint::stepper>
    ret(new rodeint::stepper(obj), true);
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::stepper_basic& obj) {
  XPtr<rodeint::stepper_basic>
    ret(new rodeint::stepper_basic(obj), true);
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::stepper_controlled& obj) {
  XPtr<rodeint::stepper_controlled>
    ret(new rodeint::stepper_controlled(obj), true);
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
rodeint::stepper as(SEXP obj) {
  XPtr<rodeint::stepper> xp(obj);
  return *xp;
}
template<>
rodeint::stepper_basic as(SEXP obj) {
  XPtr<rodeint::stepper_basic> xp(obj);
  return *xp;
}
template<>
rodeint::stepper_controlled as(SEXP obj) {
  XPtr<rodeint::stepper_controlled> xp(obj);
  return *xp;
}
}

#endif
