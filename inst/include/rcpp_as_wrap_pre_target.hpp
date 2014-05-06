#ifndef _RODEINT_RCPP_AS_WRAP_PRE_TARGET_HPP_
#define _RODEINT_RCPP_AS_WRAP_PRE_TARGET_HPP_

#include <RcppCommon.h>

// Forward declare a list of classes to expose, all within the rodeint
// namespace (not needed for files included before this one in
// rodeint.h)
namespace rodeint {
class target_cpp;
class target_r;
}

namespace Rcpp {
template<> SEXP wrap(const rodeint::target_cpp&);
template<> SEXP wrap(const rodeint::target_r&);
template<> rodeint::target_cpp as(SEXP);
template<> rodeint::target_r as(SEXP);
}

#endif
