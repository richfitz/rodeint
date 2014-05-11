#ifndef _RODEINT_ODE_SYSTEM_CLASS_HPP_
#define _RODEINT_ODE_SYSTEM_CLASS_HPP_

#include <vector>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <RcppCommon.h> // SEXP

namespace rodeint {

// Like elsewhere, it might better to template this on state_type /
// pars_type, but I just don't want to do that right now.
class ode_system_class {
public:
  typedef std::vector<double> state_type;
  typedef SEXP pars_type;
  typedef boost::function<void(const state_type&, state_type&, double)> derivs_type;
  typedef boost::function<void(pars_type)>   set_pars_type;
  typedef boost::function<pars_type()>       get_pars_type;
  ode_system_class(derivs_type derivs_,
               set_pars_type set_pars_, get_pars_type get_pars_)
    : ode_system_derivs(derivs_),
      ode_system_set_pars(set_pars_),
      ode_system_get_pars(get_pars_) {}
  void operator()(const state_type& y, state_type& dydt, const double t) {
    ode_system_derivs(y, dydt, t);
  }
  void set_pars(pars_type x) {
    ode_system_set_pars(x);
  }
  pars_type get_pars() const {
    return ode_system_get_pars();
  }
private:
  derivs_type    ode_system_derivs;
  set_pars_type  ode_system_set_pars;
  get_pars_type  ode_system_get_pars;
};

// This version of the wrapper is strongly typesafe, right down to the
// use of const correctness of functions.  That might not be ideal -
// if the evaluation of the function is const (e.g., it needs to use
// some internal function that uses local state, which seems
// reasonable) then everything falls apart.
//
// The easiest solution seems to be *not* to require a const method,
// given that people using this will have control over the use of
// const methods.  The get/set pars cases seem reasonable in their
// constness to me.
//
// As a result, ode_system_class::operator() is not a const method.
template <typename T,
          void (T::*derivs)(const ode_system_class::state_type&,
                            ode_system_class::state_type&, double) =
          &T::derivs,
          void (T::*set_pars)(ode_system_class::pars_type) = &T::set_pars,
          ode_system_class::pars_type (T::*get_pars)() const = &T::get_pars>
class wrapper {
public:
  static ode_system_class make_ode_system(T obj) {
    wrapper w(obj);
    return ode_system_class(boost::bind(derivs,   w.data, _1, _2, _3),
                            boost::bind(set_pars, w.data, _1),
                            boost::bind(get_pars, w.data));
  }
private:
  wrapper(T data_) : data(boost::shared_ptr<T>(new T(data_))) {}
  boost::shared_ptr<T> data;
};

}

#endif
