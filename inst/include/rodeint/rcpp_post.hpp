#ifndef _RODEINT_RCPP_AS_WRAP_POST_HPP_
#define _RODEINT_RCPP_AS_WRAP_POST_HPP_

// This organises loading all full class definitions, and the as/wrap
// prototypes
#include <rodeint.h>
#include "util_ublas.hpp"

namespace Rcpp {
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::ode_system_r& obj) {
  XPtr<rodeint::ode_system_r> ret(new rodeint::ode_system_r(obj), true);
  ret.attr("type") = "ode_system_r";
  ret.attr("has_jacobian") = false;
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::ode_system_cpp& obj) {
  XPtr<rodeint::ode_system_cpp> ret(new rodeint::ode_system_cpp(obj), true);
  ret.attr("type") = "ode_system_cpp";
  ret.attr("has_jacobian") = false;
  return wrap(ret);
}
template<>
inline SEXP wrap(const rodeint::ode_system_class& obj) {
  XPtr<rodeint::ode_system_class> ret(new rodeint::ode_system_class(obj), true);
  ret.attr("type") = "ode_system_class";
  ret.attr("has_jacobian") = false;
  return wrap(ret);
}

template <>
inline SEXP wrap(const rodeint::ode_system_stiff_r& obj) {
  XPtr<rodeint::ode_system_stiff_r>
    ret(new rodeint::ode_system_stiff_r(obj), true);
  ret.attr("type") = "ode_system_stiff_r";
  ret.attr("has_jacobian") = true;
  return wrap(ret);
}
template <>
inline SEXP wrap(const rodeint::ode_system_stiff_cpp& obj) {
  XPtr<rodeint::ode_system_stiff_cpp>
    ret(new rodeint::ode_system_stiff_cpp(obj), true);
  ret.attr("type") = "ode_system_stiff_cpp";
  ret.attr("has_jacobian") = true;
  return wrap(ret);
}
template <>
inline SEXP wrap(const rodeint::ode_system_stiff_class& obj) {
  XPtr<rodeint::ode_system_stiff_class>
    ret(new rodeint::ode_system_stiff_class(obj), true);
  ret.attr("type") = "ode_system_stiff_class";
  ret.attr("has_jacobian") = true;
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
inline rodeint::ode_system_r as(SEXP obj) {
  XPtr<rodeint::ode_system_r> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}
template<>
inline rodeint::ode_system_cpp as(SEXP obj) {
  XPtr<rodeint::ode_system_cpp> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}
template<>
inline rodeint::ode_system_class as(SEXP obj) {
  XPtr<rodeint::ode_system_class> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}

template<>
inline rodeint::ode_system_stiff_r as(SEXP obj) {
  XPtr<rodeint::ode_system_stiff_r> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}
template<>
inline rodeint::ode_system_stiff_cpp as(SEXP obj) {
  XPtr<rodeint::ode_system_stiff_cpp> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}
template<>
inline rodeint::ode_system_stiff_class as(SEXP obj) {
  XPtr<rodeint::ode_system_stiff_class> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}

// Not pointers, but full copies converting ublas <-> R
template<>
inline SEXP wrap(const boost::numeric::ublas::vector<double>& obj) {
  return rodeint::util::ublas_vector_to_r(obj);;
}

template<>
inline SEXP wrap(const boost::numeric::ublas::matrix<double>& obj) {
  return rodeint::util::ublas_matrix_to_r(obj);
}

template<>
inline boost::numeric::ublas::vector<double> as(SEXP obj) {
  return rodeint::util::r_vector_to_ublas<double>(as<NumericVector>(obj));
}

template<>
inline boost::numeric::ublas::matrix<double> as(SEXP obj) {
  return rodeint::util::r_matrix_to_ublas<double>(as<NumericMatrix>(obj));
}

#ifndef RODEINT_ODE_SYSTEM_ONLY
// 'wrap' definitions for each class:
template<>
inline SEXP wrap(const rodeint::stepper& obj) {
  XPtr<rodeint::stepper>
    ret(new rodeint::stepper(obj), true);
  ret.attr("type") = "stepper";
  return wrap(ret);
}

// 'as' definitions for each class:
template<>
inline rodeint::stepper as(SEXP obj) {
  XPtr<rodeint::stepper> xp(obj);
  rodeint::util::check_ptr_valid(xp);
  return *xp;
}
#endif

}

#endif
