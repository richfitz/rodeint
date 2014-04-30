#ifndef _RODEINT_RCPP_AS_WRAP_POST_HPP_
#define _RODEINT_RCPP_AS_WRAP_POST_HPP_

// This organises loading all full class definitions, and the as/wrap
// prototypes
#include "rodeint.h"

// Now it should be safe to include Rcpp.h, assuming none of the other 
#include <Rcpp.h>

namespace Rcpp {
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::ode_target_r& obj) {
  XPtr<rodeint::ode_target_r> ret(new rodeint::ode_target_r(obj), true);
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::integrator& obj) {
  XPtr<rodeint::integrator> ret(new rodeint::integrator(obj), true);
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::foo& obj) {
  XPtr<rodeint::foo> ret(new rodeint::foo(obj), true);
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
rodeint::ode_target_r as(SEXP obj) {
  XPtr<rodeint::ode_target_r> xp(obj);
  return *xp;
}
template<>
rodeint::integrator as(SEXP obj) {
  XPtr<rodeint::integrator> xp(obj);
  return *xp;
}
template<>
rodeint::foo as(SEXP obj) {
  XPtr<rodeint::foo> xp(obj);
  return *xp;
}
}

#endif
