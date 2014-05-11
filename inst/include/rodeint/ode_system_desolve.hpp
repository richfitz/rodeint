#ifndef _RODEINT_ODE_SYSTEM_DESOLVE_HPP_
#define _RODEINT_ODE_SYSTEM_DESOLVE_HPP_

#include <rodeint/ode_system_class.hpp>
#include <rodeint/util.hpp>

namespace rodeint {
namespace deSolve {

typedef void func_type(int*, double*, double*, double*, double*, int*);

class ode_system_deSolve {
public:
  typedef std::vector<double> pars_type;
  ode_system_deSolve(func_type*  func_, double* pars_deSolve_,
                     pars_type pars_)
    : func(func_), pars_deSolve(pars_deSolve_), pars(pars_) {
    copy_pars_to_deSolve();
  }
  void derivs(const std::vector<double> &y,
              std::vector<double>& dydt,
              const double t) {
    int neq = static_cast<int>(y.size());
    // De-const time and y, which requires a copy.
    double tt = t;
    std::vector<double> yy(y);
    // We don't support these arguments.
    double *yout = NULL;
    int    *ip   = NULL;
    (*func)(&neq, &tt, &yy.front(), &dydt.front(), yout, ip);
  }
  void set_pars(SEXP pars_) {
    // Lots of copying here, but probably does not matter
    pars_type tmp = Rcpp::as<pars_type>(pars_);
    util::check_length(tmp.size(), pars.size());
    pars = tmp;
    copy_pars_to_deSolve();
  }
private:
  void copy_pars_to_deSolve() {
    if (pars_deSolve == NULL) {
      Rcpp::stop("Whoa, not copying into a null ptr, crazy person");
    }
    std::copy(pars.begin(), pars.end(), pars_deSolve);
  }
  func_type* func;
  double *pars_deSolve;
  pars_type pars;
};

ode_system_class
make_ode_system_deSolve(func_type* func, double* pars_deSolve,
                        std::vector<double> pars);

}
}

#endif
