#ifndef _RODEINT_INTEGRATE_HPP_
#define _RODEINT_INTEGRATE_HPP_

#include <boost/numeric/odeint.hpp>
#include "ode_target_r.hpp"

// For now, we'll just target the ode_target_r, and then perhaps
// template this.

// No particular effort made for const correctness -- could fix that
// up.
namespace rodeint {
class integrator {
public:
  typedef ode_target_r::state_type state_type;
  typedef ode_target_r::pars_type  pars_type;
  integrator(ode_target_r target_) : target(target_) {}

  state_type derivs(const state_type& y, const double t) {
    state_type dydt(y.size());
    target(y, dydt, t);
    return dydt;
  }

  void set_target_pars(pars_type pars) {
    target.set_pars(pars);
  }

  state_type integrate(state_type y, double t0, double t1,
                       double dt) {
    last_steps =
      boost::numeric::odeint::integrate(target, y, t0, t1, dt);
    return y;
  }

  size_t get_last_steps() const {
    return last_steps;
  }
private:
  ode_target_r target;
  size_t last_steps;
};
}

#endif
