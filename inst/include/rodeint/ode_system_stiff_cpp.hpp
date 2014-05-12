#ifndef _RODEINT_ODE_SYSTEM_STIFF_CPP_HPP_
#define _RODEINT_ODE_SYSTEM_STIFF_CPP_HPP_

#include <Rcpp.h>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include "util_ublas.hpp"
#include "util.hpp" // util::check_length

// TODO: Assumes that dfdt is zero (all of the stiff systems so far
// do)

namespace rodeint {
class ode_system_stiff_cpp {
public:
  // NOTE: Different to ode_system!
  typedef boost::numeric::ublas::vector<double> state_type;
  typedef boost::numeric::ublas::matrix<double> matrix_type;
  typedef std::vector<double> pars_type;
  typedef void (*derivs_type)(const state_type&, state_type&,
                              const double, const pars_type&);
  typedef void (*jacobian_type)(const state_type&, matrix_type&,
                                const double, state_type&,
                                const pars_type&);
  ode_system_stiff_cpp(derivs_type derivs_, jacobian_type jacobian_,
                       pars_type pars_)
    : derivs(derivs_), jacobian(jacobian_), pars(pars_) {}
  void compute_derivs(const state_type &y, state_type &dydt,
                      const double t) {
    derivs(y, dydt, t, pars);
  }
  void compute_jacobian(const state_type &y, matrix_type &J,
                        const double t, state_type &dfdt) {
    jacobian(y, J, t, dfdt, pars);
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
    util::check_length(pars_.size(), pars.size());
    pars = pars_;
  }
private:
  derivs_type derivs;
  jacobian_type jacobian;
  pars_type pars;
};
}

#endif
