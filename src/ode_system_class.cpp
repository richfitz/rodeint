#include <rodeint/ode_system_class.hpp>
#include <Rcpp.h>

// [[Rcpp::export]]
rodeint::ode_system_class::pars_type
ode_system_class__get_pars(Rcpp::XPtr<rodeint::ode_system_class> ode_system) {
  return ode_system->get_pars();
}

// [[Rcpp::export]]
void
ode_system_class__set_pars(Rcpp::XPtr<rodeint::ode_system_class> ode_system,
                           rodeint::ode_system_class::pars_type pars) {
  ode_system->set_pars(pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class::state_type
ode_system_class__derivs(Rcpp::XPtr<rodeint::ode_system_class> ode_system,
                         rodeint::ode_system_class::state_type y,
                         double t) {
  rodeint::ode_system_class::state_type dydt(y.size());
  (*ode_system)(y, dydt, t);
  return dydt;
}
