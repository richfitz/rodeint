#include <rodeint_ode_system.h>
#include <R_ext/Rdynload.h>

namespace rodeint {
namespace deSolve {

static SEXP deSolve_parameters;

template <typename Target>
void run_target(double *y, double *dydt, double t, int neq) {
  typedef Rcpp::XPtr<Target> target_ptr;
  typedef typename Target::state_type state_type;

  target_ptr target(Rcpp::as<target_ptr>(deSolve_parameters));
  state_type cpp_y(y, y + neq), cpp_dydt(neq);
  (*target)(cpp_y, cpp_dydt, t);
  std::copy(cpp_dydt.begin(), cpp_dydt.end(), dydt);
}

}
}

extern "C" {
  void deSolve_func_target_class(int *neq, double *t, double *y, double *dydt,
                                 double* /* yout */, int* /* ip */) {
    rodeint::deSolve::run_target<rodeint::target_class>(y, dydt, *t, *neq);
  }

  void deSolve_func_target_cpp(int *neq, double *t, double *y, double *dydt,
                               double* /* yout */, int* /* ip */) {
    rodeint::deSolve::run_target<rodeint::target_cpp>(y, dydt, *t, *neq);
  }

  void deSolve_initfunc(void(* /* odeparms */)(int *, double *)) {
    rodeint::deSolve::deSolve_parameters =
      static_cast<SEXP>(R_GetCCallable("deSolve", "get_deSolve_gparms")());
  }
}
