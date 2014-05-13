#ifndef _RODEINT_ODE_SYSTEM_CPP_HPP_
#define _RODEINT_ODE_SYSTEM_CPP_HPP_

namespace rodeint {

template <typename OdeSystem>
struct ode_system_stiff_odeint {
  struct jacobian {
    typedef typename OdeSystem::state_type  state_type;
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
  first_type first;
  second_type second;
};

// If the above class organises inclusion, drop this
class ode_system_stiff_r;
class ode_system_stiff_cpp;
class ode_system_stiff_class;

// But these might reduce typing and errors?
typedef ode_system_stiff_odeint<ode_system_stiff_r>
ode_system_stiff_r_odeint;
typedef ode_system_stiff_odeint<ode_system_stiff_cpp>
ode_system_stiff_cpp_odeint;
typedef ode_system_stiff_odeint<ode_system_stiff_class>
ode_system_stiff_class_odeint;

}

#endif
