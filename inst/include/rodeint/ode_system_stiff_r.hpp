#ifndef _RODEINT_ODE_SYSTEM_STIFF_R_HPP_
#define _RODEINT_ODE_SYSTEM_STIFF_R_HPP_

#include <Rcpp.h>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include "util_ublas.hpp"

// TODO: Assumes that dfdt is zero (all of the stiff systems so far
// do)

namespace rodeint {
class ode_system_stiff_r {
public:
  // NOTE: Different to ode_system!
  typedef boost::numeric::ublas::vector<double> state_type;
  typedef boost::numeric::ublas::matrix<double> matrix_type;
  typedef SEXP pars_type;

  ode_system_stiff_r(Rcpp::Function derivs_, Rcpp::Function jacobian_,
                     pars_type pars_)
    : derivs(derivs_), jacobian(jacobian_), pars(pars_) {}
  void compute_derivs(const state_type& y, state_type &dydt,
                      const double t) {
    Rcpp::NumericVector ret = derivs(Rcpp::wrap(y), Rcpp::wrap(t), pars);
    std::copy(ret.begin(), ret.end(), dydt.begin());
  }
  void compute_jacobian(const state_type& y, matrix_type& J,
                        const double& t, state_type& dfdt) {
    Rcpp::NumericMatrix rJ = jacobian(Rcpp::wrap(y), Rcpp::wrap(t), pars);
    J = util::r_matrix_to_ublas<double>(rJ);
    std::fill(dfdt.begin(), dfdt.end(), 0.0);
  }
  // Compatibility with non-stiff systems:
  void operator()(const state_type& y, state_type &dydt,
                  const double t) {
    compute_derivs(y, dydt, t);
  }
  pars_type get_pars() const {
    return pars;
  }
  void set_pars(pars_type pars_) {
    pars = pars_;
  }
private:
  Rcpp::Function derivs;
  Rcpp::Function jacobian;
  pars_type pars;
};
}

#endif
