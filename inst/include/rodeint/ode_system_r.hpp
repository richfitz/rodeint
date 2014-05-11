#ifndef _RODEINT_ODE_SYSTEM_R_HPP_
#define _RODEINT_ODE_SYSTEM_R_HPP_

#include <vector>
#include <Rcpp.h>

namespace rodeint {
class ode_system_r {
public:
  typedef std::vector<double> state_type;
  typedef SEXP pars_type;
  ode_system_r(Rcpp::Function derivs_, pars_type pars_)
    : derivs(derivs_), pars(pars_) {}
  void operator()(const state_type& y, state_type &dydt,
                  const double t) {
    const state_type ret =
      Rcpp::as<state_type>(derivs(Rcpp::wrap(y), Rcpp::wrap(t), pars));
    std::copy(ret.begin(), ret.end(), dydt.begin());
  }
  pars_type get_pars() const {
    return pars;
  }
  void set_pars(pars_type pars_) {
    pars = pars_;
  }
private:
  Rcpp::Function derivs;
  pars_type pars;
};
}

#endif
