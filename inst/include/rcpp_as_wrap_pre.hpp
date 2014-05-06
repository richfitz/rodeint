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

// The target_r and target_cpp definitions are done elsewhere
#include "rcpp_as_wrap_pre_target.hpp"

namespace Rcpp {
// A 'wrap' prototype for each class:
template<> SEXP wrap(const rodeint::stepper&);
template<> SEXP wrap(const rodeint::stepper_basic&);
template<> SEXP wrap(const rodeint::stepper_controlled&);

// An 'as' prototype for each class:
template<> rodeint::stepper as(SEXP);
template<> rodeint::stepper_basic as(SEXP);
template<> rodeint::stepper_controlled as(SEXP);
}

#endif
