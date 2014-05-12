#ifndef _RODEINT_RCPP_AS_WRAP_PRE_HPP_
#define _RODEINT_RCPP_AS_WRAP_PRE_HPP_

// This file, and rcpp_as_wrap_post, do a fairly awkward dance where
//
// * the wrap/as template specifications need to be loaded before
//   Rcpp is loaded.
// * but the wrap/as functions *use* the Rcpp functions
// * so do some of the class definitions themselves!
//
// Sometimes just doing both the wrap and as definitions after loading
// Rcpp seems to work, but this way we're playing by the rules and
// everything should be OK at the cost of some ugliness.

#include <RcppCommon.h>

// Types we need later
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>

// Forward declare a list of classes to expose, all within the rodeint
// namespace (not needed for files included before this one in
// rodeint.h)
namespace rodeint {
class ode_system_class;
class ode_system_cpp;
class ode_system_r;

class ode_system_stiff_r;
}

namespace Rcpp {
template<> SEXP wrap(const rodeint::ode_system_class&);
template<> SEXP wrap(const rodeint::ode_system_cpp&);
template<> SEXP wrap(const rodeint::ode_system_r&);

template<> SEXP wrap(const rodeint::ode_system_stiff_r&);

template<> rodeint::ode_system_class as(SEXP);
template<> rodeint::ode_system_cpp as(SEXP);
template<> rodeint::ode_system_r as(SEXP);

template<> rodeint::ode_system_stiff_r as(SEXP);

template<> SEXP wrap(const boost::numeric::ublas::vector<double>&);
template<> SEXP wrap(const boost::numeric::ublas::matrix<double>&);
template<> boost::numeric::ublas::vector<double> as(SEXP);
template<> boost::numeric::ublas::matrix<double> as(SEXP);

#ifndef RODEINT_ODE_SYSTEM_ONLY
template<> SEXP wrap(const rodeint::stepper&);
template<> rodeint::stepper as(SEXP);
#endif
}

#endif
