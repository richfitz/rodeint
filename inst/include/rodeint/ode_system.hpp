#ifndef _RODEINT_ODE_SYSTEM_HPP_
#define _RODEINT_ODE_SYSTEM_HPP_

#include <Rcpp.h>

namespace rodeint {

template <typename OdeSystem>
SEXP ode_system__get_pars(Rcpp::XPtr<OdeSystem> ode_system) {
  return Rcpp::wrap(ode_system->get_pars());
}

template <typename OdeSystem>
void ode_system__set_pars(Rcpp::XPtr<OdeSystem> ode_system, SEXP pars) {
  ode_system->set_pars(Rcpp::as<typename OdeSystem::pars_type>(pars));
}

template <typename OdeSystem>
typename OdeSystem::state_type
ode_system__derivs(Rcpp::XPtr<OdeSystem> ode_system,
                   typename OdeSystem::state_type y,
                   double t) {
  typename OdeSystem::state_type dydt(y.size());
  (*ode_system)(y, dydt, t);
  return dydt;
}

// Stiff systems only:
template <typename OdeSystem>
typename OdeSystem::matrix_type
ode_system__jacobian(Rcpp::XPtr<OdeSystem> ode_system,
                     typename OdeSystem::state_type y,
                     double t) {
  const size_t n = y.size();
  typename OdeSystem::state_type  dfdt(n);
  typename OdeSystem::matrix_type J(n, n);
  ode_system->compute_jacobian(y, J, t, dfdt);
  return J;
}

}

#endif
