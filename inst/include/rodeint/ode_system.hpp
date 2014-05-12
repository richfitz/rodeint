#ifndef _RODEINT_ODE_SYSTEM_HPP_
#define _RODEINT_ODE_SYSTEM_HPP_

#include <Rcpp.h>

namespace rodeint {

template <typename OdeSystem>
SEXP ode_system__get_pars(SEXP ode_system) {
  typedef Rcpp::XPtr<OdeSystem> ptr_type;
  ptr_type ode_system_ptr(Rcpp::as<ptr_type>(ode_system));
  return Rcpp::wrap(ode_system_ptr->get_pars());
}

template <typename OdeSystem>
void ode_system__set_pars(SEXP ode_system, SEXP pars) {
  typedef Rcpp::XPtr<OdeSystem> ptr_type;
  ptr_type ode_system_ptr(Rcpp::as<ptr_type>(ode_system));
  ode_system_ptr->set_pars(Rcpp::as<typename OdeSystem::pars_type>(pars));
}

template <typename OdeSystem>
typename OdeSystem::state_type
ode_system__derivs(SEXP ode_system,
                   typename OdeSystem::state_type y,
                   double t) {
  typedef Rcpp::XPtr<OdeSystem> ptr_type;
  ptr_type ode_system_ptr(Rcpp::as<ptr_type>(ode_system));
  typename OdeSystem::state_type dydt(y.size());
  (*ode_system_ptr)(y, dydt, t);
  return dydt;
}

// Stiff systems only:
template <typename OdeSystem>
typename OdeSystem::matrix_type
ode_system__jacobian(SEXP ode_system,
                     typename OdeSystem::state_type y,
                     double t) {
  typedef Rcpp::XPtr<OdeSystem> ptr_type;
  ptr_type ode_system_ptr(Rcpp::as<ptr_type>(ode_system));
  const size_t n = y.size();
  typename OdeSystem::state_type  dfdt(n);
  typename OdeSystem::matrix_type J(n, n);
  ode_system_ptr->compute_jacobian(y, J, t, dfdt);
  return J;
}

}

#endif
