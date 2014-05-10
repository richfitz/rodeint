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

// Forward declare a list of classes to expose, all within the rodeint
// namespace (not needed for files included before this one in
// rodeint.h)
namespace rodeint {
class target_class;
class target_cpp;
class target_r;
}

namespace Rcpp {
template<> SEXP wrap(const rodeint::target_class&);
template<> SEXP wrap(const rodeint::target_cpp&);
template<> SEXP wrap(const rodeint::target_r&);
template<> rodeint::target_class as(SEXP);
template<> rodeint::target_cpp as(SEXP);
template<> rodeint::target_r as(SEXP);

#ifndef RODEINT_TARGET_ONLY
template<> SEXP wrap(const rodeint::stepper&);
template<> rodeint::stepper as(SEXP);
#endif
}

#endif
