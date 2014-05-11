#include <rodeint/ode_system_cpp.hpp>

// TODO: The wrappers around derivs, get_pars and set_pars are
// identical, and we can just template the repetition somewhere and
// have much shorter functions here (like with integrate).

// [[Rcpp::export]]
rodeint::ode_system_cpp::state_type
ode_system_cpp__derivs(Rcpp::XPtr<rodeint::ode_system_cpp> ode_system,
                       rodeint::ode_system_cpp::state_type y,
                       double t) {
  rodeint::ode_system_cpp::state_type dydt(y.size());
  (*ode_system)(y, dydt, t);
  return dydt;
}

// [[Rcpp::export]]
rodeint::ode_system_cpp::pars_type
ode_system_cpp__get_pars(Rcpp::XPtr<rodeint::ode_system_cpp> ode_system) {
  return ode_system->get_pars();
}

// [[Rcpp::export]]
void
ode_system_cpp__set_pars(Rcpp::XPtr<rodeint::ode_system_cpp> ode_system,
                         rodeint::ode_system_cpp::pars_type pars) {
  ode_system->set_pars(pars);
}
