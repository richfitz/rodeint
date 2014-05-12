#ifndef _RODEINT_ODE_SYSTEM_STIFF_CLASS_HPP_
#define _RODEINT_ODE_SYSTEM_STIFF_CLASS_HPP_

#include <Rcpp.h>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include "util_ublas.hpp"
#include "util.hpp" // util::check_length

#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>

namespace rodeint {

class ode_system_stiff_class {
public:
  // NOTE: Different to ode_system!
  typedef boost::numeric::ublas::vector<double> state_type;
  typedef boost::numeric::ublas::matrix<double> matrix_type;
  typedef SEXP pars_type;
  typedef boost::function<void(const state_type&, state_type&,
                               const double)> derivs_type;
  typedef boost::function<void(const state_type&, matrix_type&,
                               const double, state_type&)> jacobian_type;
  typedef boost::function<void(pars_type)> set_pars_type;
  ode_system_stiff_class(derivs_type derivs_, jacobian_type jacobian_,
                         set_pars_type set_pars_, SEXP pars_)
    : ode_system_derivs(derivs_),
      ode_system_jacobian(jacobian_),
      ode_system_set_pars(set_pars_),
      pars(pars_) {}
  void compute_derivs(const state_type &y, state_type &dydt,
                      const double t) {
    ode_system_derivs(y, dydt, t);
  }
  void compute_jacobian(const state_type &y, matrix_type &J,
                        const double t, state_type &dfdt) {
    ode_system_jacobian(y, J, t, dfdt);
  }
  // Compatibility with non-stiff systems:
  void operator()(const state_type& y, state_type &dydt,
                  const double t) {
    compute_derivs(y, dydt, t);
  }
  void set_pars(SEXP pars_) {
    ode_system_set_pars(pars_);
    pars = pars_;
  }
  SEXP get_pars() const {
    return Rcpp::wrap(pars);
  }
private:
  derivs_type   ode_system_derivs;
  jacobian_type ode_system_jacobian;
  set_pars_type ode_system_set_pars;
  Rcpp::RObject pars;
};

// TODO: Here and in system_class check the version compiles with
// different target names.
namespace internals {
template <typename T,
          void (T::*derivs)(const ode_system_stiff_class::state_type&,
                            ode_system_stiff_class::state_type&,
                            const double) =
          &T::derivs,
          void (T::*jacobian)(const ode_system_stiff_class::state_type&,
                              ode_system_stiff_class::matrix_type&,
                              const double,
                              ode_system_stiff_class::state_type&) =
          &T::jacobian,
          void (T::*set_pars)(ode_system_stiff_class::pars_type) =
          &T::set_pars>
class stiff_wrapper {
public:
  static ode_system_stiff_class make_ode_system(T obj, SEXP pars) {
    stiff_wrapper w(obj);
    return ode_system_stiff_class(boost::bind(derivs,   w.data, _1, _2, _3),
                                  boost::bind(jacobian, w.data, _1, _2, _3, _4),
                                  boost::bind(set_pars, w.data, _1),
                                  pars);
  }
private:
  stiff_wrapper(T data_) : data(boost::shared_ptr<T>(new T(data_))) {}
  boost::shared_ptr<T> data;
};
}

template <typename T>
ode_system_stiff_class
make_ode_system_stiff_class(typename T::pars_type pars) {
  T obj(pars);
  return internals::stiff_wrapper<T>::make_ode_system(obj, Rcpp::wrap(pars));
}

template <typename T>
ode_system_stiff_class
make_ode_system_stiff_class(typename T::pars_type pars, T obj) {
  return internals::stiff_wrapper<T>::make_ode_system(obj, Rcpp::wrap(pars));
}

}

#endif
