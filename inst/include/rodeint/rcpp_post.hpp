#ifndef _RODEINT_RCPP_AS_WRAP_POST_HPP_
#define _RODEINT_RCPP_AS_WRAP_POST_HPP_

// This organises loading all full class definitions, and the as/wrap
// prototypes
#include <rodeint.h>

namespace Rcpp {
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::ode_system_r& obj) {
  XPtr<rodeint::ode_system_r> ret(new rodeint::ode_system_r(obj), true);
  ret.attr("type") = "ode_system_r";
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::ode_system_cpp& obj) {
  XPtr<rodeint::ode_system_cpp> ret(new rodeint::ode_system_cpp(obj), true);
  ret.attr("type") = "ode_system_cpp";
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::ode_system_class& obj) {
  XPtr<rodeint::ode_system_class> ret(new rodeint::ode_system_class(obj), true);
  ret.attr("type") = "ode_system_class";
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
inline rodeint::ode_system_class as(SEXP obj) {
  XPtr<rodeint::ode_system_class> xp(obj);
  return *xp;
}
template<>
inline rodeint::ode_system_cpp as(SEXP obj) {
  XPtr<rodeint::ode_system_cpp> xp(obj);
  return *xp;
}
template<>
inline rodeint::ode_system_r as(SEXP obj) {
  XPtr<rodeint::ode_system_r> xp(obj);
  return *xp;
}

#ifndef RODEINT_ODE_SYSTEM_ONLY
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::stepper& obj) {
  XPtr<rodeint::stepper>
    ret(new rodeint::stepper(obj), true);
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
inline rodeint::stepper as(SEXP obj) {
  XPtr<rodeint::stepper> xp(obj);
  return *xp;
}
#endif

}

#endif
