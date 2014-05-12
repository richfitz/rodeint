#ifndef _RODEINT_ODE_SYSTEM_CLASS_HPP_
#define _RODEINT_ODE_SYSTEM_CLASS_HPP_

#include <vector>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <Rcpp.h> // SEXP, Rcpp::as

// TODO: Assumes that dfdt is zero (all of the stiff systems so far
// do)

namespace rodeint {

class ode_system_class {
public:
  typedef SEXP pars_type;
  typedef std::vector<double> state_type;
  typedef boost::function<void(const state_type&, state_type&,
                               const double)> derivs_type;
  typedef boost::function<void(pars_type)> set_pars_type;
  ode_system_class(derivs_type derivs_, set_pars_type set_pars_,
                   SEXP pars_)
    : ode_system_derivs(derivs_),
      ode_system_set_pars(set_pars_),
      pars(pars_) {}
  void operator()(const state_type& y, state_type& dydt, const double t) {
    ode_system_derivs(y, dydt, t);
  }
  void set_pars(SEXP pars_) {
    ode_system_set_pars(pars_);
    pars = pars_;
  }
  SEXP get_pars() const {
    return Rcpp::wrap(pars);
  }
private:
  derivs_type    ode_system_derivs;
  set_pars_type  ode_system_set_pars;
  Rcpp::RObject pars;
};

// This wrapper is strongly typesafe, right down to the use of const
// correctness of functions.  That might not be ideal - if the
// evaluation of the function is const (e.g., it needs to use some
// internal function that uses local state, which seems reasonable)
// then everything falls apart.
//
// The easiest solution seems to be *not* to require a const method,
// given that people using this will have control over the use of
// const methods.  The get/set pars cases seem reasonable in their
// constness to me.
//
// As a result, ode_system_class::operator() is not a const method.
namespace internals {
template <typename T,
          void (T::*derivs)(const ode_system_class::state_type&,
                            ode_system_class::state_type&,
                            const double) =
          &T::derivs,
          void (T::*set_pars)(ode_system_class::pars_type) =
          &T::set_pars>
class wrapper {
public:
  static ode_system_class make_ode_system(T obj, SEXP pars) {
    wrapper w(obj);
    return ode_system_class(boost::bind(derivs,   w.data, _1, _2, _3),
                            boost::bind(set_pars, w.data, _1),
                            pars);
  }
private:
  wrapper(T data_) : data(boost::shared_ptr<T>(new T(data_))) {}
  boost::shared_ptr<T> data;
};
}

// NOTE:
// If we are on a C++11 compatible compiler we can use the same
// default template arguments as above to allow different member
// functions to be specified, and therefore only need the first
// version.  Add "= &T::derivs" and "= &T::set_pars" after the second
// and third template parameters.
//
// It can be used as:
//   make_ode_system_stiff_class<my_system>(pars);
// or
//   make_ode_system_stiff_class<my_system, &my_system::derivs,
//                               &my_system::set_pars>(pars);
// see inst/examples/defaults.cpp for examples.

// Version for classes with more than one parameter in their
// constructor.
template <typename T,
          void (T::*derivs)(const ode_system_class::state_type&,
                            ode_system_class::state_type&,
                            const double),
          void (T::*set_pars)(ode_system_class::pars_type)>
ode_system_class make_ode_system_class(T obj, typename T::pars_type pars) {
  SEXP p = Rcpp::wrap(pars);
  return internals::wrapper<T, derivs, set_pars>::make_ode_system(obj, p);
}

// Version assuming defaults (this is easier with C++11)
template <typename T>
ode_system_class make_ode_system_class(T obj, typename T::pars_type pars) {
  return make_ode_system_class<T, &T::derivs, &T::set_pars>(obj, pars);
}

// Version assuming object takes just pars as argument to constructor:
template <typename T,
          void (T::*derivs)(const ode_system_class::state_type&,
                            ode_system_class::state_type&,
                            const double),
          void (T::*set_pars)(ode_system_class::pars_type)>
ode_system_class make_ode_system_class(typename T::pars_type pars) {
  T obj(pars);
  return make_ode_system_class<T, derivs, set_pars>(obj, pars);
}

// And version assuming object takes pars *and* assuming defaults.
template <typename T>
ode_system_class make_ode_system_class(typename T::pars_type pars) {
  T obj(pars);
  return make_ode_system_class(obj, pars);
}

}

#endif
