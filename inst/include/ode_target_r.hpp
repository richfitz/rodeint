#ifndef _RODEINT_ODE_TARGET_R_HPP_
#define _RODEINT_ODE_TARGET_R_HPP_

#include <vector>
#include <Rcpp.h>

namespace rodeint {
class ode_target_r {
public:
  typedef std::vector<double> state_type;
  typedef SEXP pars_type;
  ode_target_r(Rcpp::Function derivs_, pars_type pars_);
  void operator()(const state_type& y, state_type &dydt,
                  const double t);
  void set_pars(pars_type pars_);
private:
  Rcpp::Function derivs;
  pars_type pars;
};
}

#endif
