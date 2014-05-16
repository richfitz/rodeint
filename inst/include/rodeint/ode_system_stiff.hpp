#ifndef _RODEINT_ODE_SYSTEM_CPP_HPP_
#define _RODEINT_ODE_SYSTEM_CPP_HPP_

namespace rodeint {

// TODO: Merge into integrate_stiff

// See
//   http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/concepts/implicit_system.html
// for why this works.
template <typename OdeSystem>
struct ode_system_stiff_odeint {
  typedef typename OdeSystem::state_type  state_type;
  struct jacobian {
    typedef typename OdeSystem::matrix_type matrix_type;
    jacobian(OdeSystem ode_system_) : ode_system(ode_system_) {}
    void operator()(const state_type& y, matrix_type& J ,
                    const double t, state_type& dfdt ) {
      ode_system.compute_jacobian(y, J, t, dfdt);
    }
    OdeSystem ode_system;
  };
  typedef OdeSystem first_type;
  typedef jacobian second_type;
  ode_system_stiff_odeint(OdeSystem ode_system)
    : first(ode_system), second(jacobian(ode_system)) {}
  // This allows this to be used by non-implicit solvers
  void operator()(const state_type& y, state_type &dydt,
                  const double t) {
    first(y, dydt, t);
  }
  first_type first;
  second_type second;
};

}

#endif
