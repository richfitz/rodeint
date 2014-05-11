#ifndef _RODEINT_RCPP_AS_WRAP_POST_HPP_
#define _RODEINT_RCPP_AS_WRAP_POST_HPP_

// This organises loading all full class definitions, and the as/wrap
// prototypes
#include <rodeint.h>

namespace Rcpp {
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::target_class& obj) {
  XPtr<rodeint::target_class> ret(new rodeint::target_class(obj), true);
  ret.attr("type") = "target_class";
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::target_cpp& obj) {
  XPtr<rodeint::target_cpp> ret(new rodeint::target_cpp(obj), true);
  ret.attr("type") = "target_cpp";
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::target_r& obj) {
  XPtr<rodeint::target_r> ret(new rodeint::target_r(obj), true);
  ret.attr("type") = "target_r";
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

#ifndef RODEINT_TARGET_ONLY
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
