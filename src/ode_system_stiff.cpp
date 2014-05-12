#include <rodeint/ode_system_stiff_r.hpp>
#include <rodeint/ode_system_stiff_cpp.hpp>
#include <rodeint/ode_system.hpp>

// 1: r
// [[Rcpp::export]]
SEXP
ode_system_stiff_r__get_pars(Rcpp::XPtr<rodeint::ode_system_stiff_r> ode_system) {
  return rodeint::ode_system__get_pars<rodeint::ode_system_stiff_r>(ode_system);
}

// [[Rcpp::export]]
void
ode_system_stiff_r__set_pars(Rcpp::XPtr<rodeint::ode_system_stiff_r> ode_system,
                       SEXP pars) {
  rodeint::ode_system__set_pars<rodeint::ode_system_stiff_r>(ode_system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_stiff_r::state_type
ode_system_stiff_r__derivs(Rcpp::XPtr<rodeint::ode_system_stiff_r> ode_system,
                           rodeint::ode_system_stiff_r::state_type y,
                           double t) {
  return rodeint::ode_system__derivs<rodeint::ode_system_stiff_r>(ode_system, y, t);
}

// [[Rcpp::export]]
rodeint::ode_system_stiff_r::matrix_type
ode_system_stiff_r__jacobian(Rcpp::XPtr<rodeint::ode_system_stiff_r> ode_system,
                             rodeint::ode_system_stiff_r::state_type y,
                             double t) {
  return rodeint::ode_system__jacobian<rodeint::ode_system_stiff_r>(ode_system, y, t);
}

// Also need this one for the R system only.
// [[Rcpp::export]]
rodeint::ode_system_stiff_r ode_system_stiff_r__ctor(Rcpp::Function derivs,
                                                     Rcpp::Function jacobian,
                                                     SEXP pars) {
  return rodeint::ode_system_stiff_r(derivs, jacobian, pars);
}

// 2: cpp
// [[Rcpp::export]]
SEXP
ode_system_stiff_cpp__get_pars(Rcpp::XPtr<rodeint::ode_system_stiff_cpp> ode_system) {
  return rodeint::ode_system__get_pars<rodeint::ode_system_stiff_cpp>(ode_system);
}

// [[Rcpp::export]]
void
ode_system_stiff_cpp__set_pars(Rcpp::XPtr<rodeint::ode_system_stiff_cpp> ode_system,
                       SEXP pars) {
  rodeint::ode_system__set_pars<rodeint::ode_system_stiff_cpp>(ode_system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_stiff_cpp::state_type
ode_system_stiff_cpp__derivs(Rcpp::XPtr<rodeint::ode_system_stiff_cpp> ode_system,
                           rodeint::ode_system_stiff_cpp::state_type y,
                           double t) {
  return rodeint::ode_system__derivs<rodeint::ode_system_stiff_cpp>(ode_system, y, t);
}

// [[Rcpp::export]]
rodeint::ode_system_stiff_cpp::matrix_type
ode_system_stiff_cpp__jacobian(Rcpp::XPtr<rodeint::ode_system_stiff_cpp> ode_system,
                             rodeint::ode_system_stiff_cpp::state_type y,
                             double t) {
  return rodeint::ode_system__jacobian<rodeint::ode_system_stiff_cpp>(ode_system, y, t);
}
