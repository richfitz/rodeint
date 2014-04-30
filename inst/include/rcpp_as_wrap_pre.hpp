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

// List of classes to expose, all within the rodeint namespace.
namespace rodeint {
class ode_target_r;
class integrator;
struct foo;
}

namespace Rcpp {
// A 'wrap' prototype for each class:
template<> SEXP wrap(const rodeint::ode_target_r&);
template<> SEXP wrap(const rodeint::integrator&);
template<> SEXP wrap(const rodeint::foo&);

// An 'as' prototype for each class:
template<> rodeint::ode_target_r as(SEXP);
template<> rodeint::integrator as(SEXP);
template<> rodeint::foo as(SEXP);
}

#endif
