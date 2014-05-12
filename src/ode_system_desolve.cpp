#include <rodeint/ode_system_desolve.hpp>
#include <rodeint/ode_system_cpp.hpp>

namespace rodeint {
namespace deSolve {

ode_system_class
make_ode_system_deSolve(func_type* func, double* pars_deSolve,
                        std::vector<double> pars) {
  rodeint::deSolve::ode_system_deSolve obj(func, pars_deSolve, pars);
  return rodeint::make_ode_system_class(obj, pars);
}

// The inverse: use rodeint's objective in deSolve, so this all needs
// C linkage

static SEXP deSolve_parameters;

template <typename OdeSystem>
void run_ode_system(double *y, double *dydt, double t, int neq) {
  typedef Rcpp::XPtr<OdeSystem> ode_system_ptr;
  typedef typename OdeSystem::state_type state_type;

  ode_system_ptr ode_system(Rcpp::as<ode_system_ptr>(deSolve_parameters));
  state_type cpp_y(y, y + neq), cpp_dydt(neq);
  (*ode_system)(cpp_y, cpp_dydt, t);
  std::copy(cpp_dydt.begin(), cpp_dydt.end(), dydt);
}

}
}

extern "C" {
  using namespace rodeint;
  void deSolve_func_ode_system_class(int *neq, double *t, double *y,
                                     double *dydt,
                                     double* /*yout*/, int* /*ip*/) {
    deSolve::run_ode_system<ode_system_class>(y, dydt, *t, *neq);
  }

  void deSolve_func_ode_system_cpp(int *neq, double *t, double *y,
                                   double *dydt,
                                   double* /* yout */, int* /* ip */) {
    deSolve::run_ode_system<ode_system_cpp>(y, dydt, *t, *neq);
  }

  void deSolve_initfunc(void(* /* odeparms */)(int *, double *)) {
    deSolve::deSolve_parameters =
      static_cast<SEXP>(R_GetCCallable("deSolve", "get_deSolve_gparms")());
  }
}
