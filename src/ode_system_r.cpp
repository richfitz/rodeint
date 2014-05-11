#include <rodeint/ode_system_r.hpp>

// [[Rcpp::export]]
rodeint::ode_system_r ode_system_r__ctor(Rcpp::Function derivs,
                                         SEXP pars_type) {
  return rodeint::ode_system_r(derivs, pars_type);
}

// [[Rcpp::export]]
rodeint::ode_system_r::state_type
ode_system_r__derivs(Rcpp::XPtr<rodeint::ode_system_r> ode_system,
                     rodeint::ode_system_r::state_type y,
                     double t) {
  rodeint::ode_system_r::state_type dydt(y.size());
  (*ode_system)(y, dydt, t);
  return dydt;
}

// [[Rcpp::export]]
SEXP ode_system_r__get_pars(Rcpp::XPtr<rodeint::ode_system_r> ode_system) {
  return ode_system->get_pars();
}

// [[Rcpp::export]]
void ode_system_r__set_pars(Rcpp::XPtr<rodeint::ode_system_r> ode_system,
                            SEXP pars) {
  ode_system->set_pars(pars);
}
