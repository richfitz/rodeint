#include <rodeint/ode_system_r.hpp>
#include <rodeint/ode_system_cpp.hpp>
#include <rodeint/ode_system_class.hpp>

#include <rodeint/ode_system.hpp>

// 1: r
// [[Rcpp::export]]
SEXP ode_system_r__get_pars(rodeint::ode_system_r system) {
  return rodeint::ode_system__get_pars(system);
}

// [[Rcpp::export]]
void
ode_system_r__set_pars(Rcpp::XPtr<rodeint::ode_system_r> system,
                       SEXP pars) {
  rodeint::ode_system__set_pars(system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_r::state_type
ode_system_r__derivs(rodeint::ode_system_r system,
                     rodeint::ode_system_r::state_type y, double t) {
  return rodeint::ode_system__derivs(system, y, t);
}

// Also need this one for the R system only.
// [[Rcpp::export]]
rodeint::ode_system_r ode_system_r__ctor(Rcpp::Function derivs,
                                         SEXP pars) {
  return rodeint::ode_system_r(derivs, pars);
}

// 2: cpp
// [[Rcpp::export]]
SEXP ode_system_cpp__get_pars(rodeint::ode_system_cpp system) {
  return rodeint::ode_system__get_pars(system);
}

// [[Rcpp::export]]
void
ode_system_cpp__set_pars(Rcpp::XPtr<rodeint::ode_system_cpp> system,
                         SEXP pars) {
  rodeint::ode_system__set_pars(system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_cpp::state_type
ode_system_cpp__derivs(rodeint::ode_system_cpp system,
                     rodeint::ode_system_cpp::state_type y, double t) {
  return rodeint::ode_system__derivs(system, y, t);
}

// 3: class
// [[Rcpp::export]]
SEXP ode_system_class__get_pars(rodeint::ode_system_class system) {
  return rodeint::ode_system__get_pars(system);
}

// [[Rcpp::export]]
void
ode_system_class__set_pars(Rcpp::XPtr<rodeint::ode_system_class> system,
                           SEXP pars) {
  rodeint::ode_system__set_pars(system, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class::state_type
ode_system_class__derivs(rodeint::ode_system_class system,
                     rodeint::ode_system_class::state_type y, double t) {
  return rodeint::ode_system__derivs(system, y, t);
}
