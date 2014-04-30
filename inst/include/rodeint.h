#ifndef _RODEINT_RCPP_AS_WRAP_PRE_H_
#define _RODEINT_RCPP_AS_WRAP_PRE_H_

// NOTE: This file must be a ".h" file, not a ".hpp" file to work with
// Rcpp attributes.

// This one goes first, because as/wrap prototypes must preceed Rcpp.h
#include "rcpp_as_wrap_pre.hpp"

// Then the actual code from the package:
#include "ode_target_r.hpp"
#include "integrate.hpp"
#include "pointer.hpp"

// And then the actual as/wrap definitions now that all classes have
// been defined.
#include "rcpp_as_wrap_post.hpp"

#endif
