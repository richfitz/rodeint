#ifndef _RODEINT_TARGET_CPP_HPP_
#define _RODEINT_TARGET_CPP_HPP_

#include <vector>
#include <Rcpp.h>
#include <rodeint/util.hpp>

namespace rodeint {

class target_cpp {
public:
  typedef std::vector<double> state_type;
  typedef std::vector<double> pars_type;
  typedef void (*derivs_type)(const state_type&, state_type&, const double,
                              const pars_type&);
  target_cpp(derivs_type derivs_, pars_type pars_)
    : derivs(derivs_), pars(pars_) {}
  void operator()(const state_type& y, state_type &dydt,
                  const double t) {
    derivs(y, dydt, t, pars);
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
  pars_type pars;
};
}

#endif
