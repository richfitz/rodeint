#ifndef _RODEINT_TARGET_C_HPP_
#define _RODEINT_TARGET_C_HPP_

#include <vector>
#include <Rcpp.h>

// There are a couple of ways that this might be useful to initialise
// -- one of them is going to be directly, given a free function,
// though that ight be better templated?  Something like
//
//   target_c<myfunc> target;
//
// That seems to be the most C++ish way.  However, that doesn't really
// work because that requires compile time fucking around and we want
// this to work regardless.
//
// I'm writing this currently to work with pure C++, no C style things
// at all.  But I feel like this is all a bit of a clusterfuck.
namespace rodeint {

class target_c {
public:
  typedef std::vector<double> state_type;
  typedef std::vector<double> pars_type;
  typedef void (*derivs_type)(const state_type&, state_type&, const double,
                              const pars_type&);
  target_c(derivs_type derivs_, pars_type pars_)
    : derivs(derivs_), pars(pars_) {}
  void operator()(const state_type& y, state_type &dydt,
                  const double t) {
    derivs(y, dydt, t, pars);
  }
  pars_type get_pars() const {
    return pars;
  }
  // TODO: control over checking the number of parameters, perhaps
  // flagged by a parameter to the constructor (fixed_parameter_length)
  void set_pars(pars_type pars_) {
    pars = pars_;
  }
private:
  derivs_type derivs;
  pars_type pars;
};
}

#endif
