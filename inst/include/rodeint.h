// -*-c++-*-
#ifndef _RODEINT_H_
#define _RODEINT_H_

// NOTE: This file must be a ".h" file, not a ".hpp" file to work with
// Rcpp attributes.

// These files do not reference Rcpp, and so can be included first.
// Some of these are needed by rcpp_as_wrap_pre as they define types
// used within, saving the need for forward declaration.
#include "stepper_controlled.hpp"

// This one goes first, because as/wrap prototypes must preceed
// Rcpp.h -- after this point anything is allowed to include Rcpp
// without causing a hassle.
#include "rcpp_as_wrap_pre.hpp"

// Then the actual code from the package:
#include "target_cpp.hpp"
#include "target_r.hpp"

// And then the actual as/wrap definitions now that all classes have
// been defined.
#include "rcpp_as_wrap_post.hpp"

#endif
