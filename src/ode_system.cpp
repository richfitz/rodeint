#include <rodeint/ode_system_r.hpp>
#include <rodeint/ode_system_cpp.hpp>
#include <rodeint/ode_system_class.hpp>

#include <rodeint/ode_system.hpp>

// TODO: As with integrate.cpp, this file would be substantially
// easier to look after with code generation.

// 1: r
// [[Rcpp::export]]
rodeint::ode_system_r::pars_type
ode_system_r__get_pars(Rcpp::XPtr<rodeint::ode_system_r> ode_system) {
  return rodeint::ode_system__get_pars<rodeint::ode_system_r>(ode_system);
}

// [[Rcpp::export]]
void
ode_system_r__set_pars(Rcpp::XPtr<rodeint::ode_system_r> ode_system,
                       rodeint::ode_system_r::pars_type pars) {
  rodeint::ode_system__set_pars<rodeint::ode_system_r>(ode_system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_r::state_type
ode_system_r__derivs(Rcpp::XPtr<rodeint::ode_system_r> ode_system,
                     rodeint::ode_system_r::state_type y,
                     double t) {
  return rodeint::ode_system__derivs<rodeint::ode_system_r>(ode_system, y, t);
}

// Also need this one for the R system only.
// [[Rcpp::export]]
rodeint::ode_system_r ode_system_r__ctor(Rcpp::Function derivs,
                                         SEXP pars_type) {
  return rodeint::ode_system_r(derivs, pars_type);
}

// 2: cpp
// [[Rcpp::export]]
rodeint::ode_system_cpp::pars_type
ode_system_cpp__get_pars(Rcpp::XPtr<rodeint::ode_system_cpp> ode_system) {
  return rodeint::ode_system__get_pars<rodeint::ode_system_cpp>(ode_system);
}

// [[Rcpp::export]]
void
ode_system_cpp__set_pars(Rcpp::XPtr<rodeint::ode_system_cpp> ode_system,
                         rodeint::ode_system_cpp::pars_type pars) {
  rodeint::ode_system__set_pars<rodeint::ode_system_cpp>(ode_system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_cpp::state_type
ode_system_cpp__derivs(Rcpp::XPtr<rodeint::ode_system_cpp> ode_system,
                       rodeint::ode_system_cpp::state_type y,
                       double t) {
  return rodeint::ode_system__derivs<rodeint::ode_system_cpp>(ode_system, y, t);
}

// 3: class
// [[Rcpp::export]]
rodeint::ode_system_class::pars_type
ode_system_class__get_pars(Rcpp::XPtr<rodeint::ode_system_class> ode_system) {
  return rodeint::ode_system__get_pars<rodeint::ode_system_class>(ode_system);
}

// [[Rcpp::export]]
void
ode_system_class__set_pars(Rcpp::XPtr<rodeint::ode_system_class> ode_system,
                           rodeint::ode_system_class::pars_type pars) {
  rodeint::ode_system__set_pars<rodeint::ode_system_class>(ode_system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class::state_type
ode_system_class__derivs(Rcpp::XPtr<rodeint::ode_system_class> ode_system,
                         rodeint::ode_system_class::state_type y,
                         double t) {
  return rodeint::ode_system__derivs<rodeint::ode_system_class>(ode_system, y, t);
}
