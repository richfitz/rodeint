#ifndef _RODEINT_ODE_SYSTEM_HPP_
#define _RODEINT_ODE_SYSTEM_HPP_

#include <Rcpp.h>

namespace rodeint {

template <typename OdeSystem>
typename OdeSystem::pars_type
ode_system__get_pars(Rcpp::XPtr<OdeSystem> ode_system) {
  return ode_system->get_pars();
}

template <typename OdeSystem>
void
ode_system__set_pars(Rcpp::XPtr<OdeSystem> ode_system,
                     typename OdeSystem::pars_type pars) {
  ode_system->set_pars(pars);
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

}

#endif
