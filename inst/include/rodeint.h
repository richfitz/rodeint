// -*-c++-*-
#ifndef _RODEINT_H_
#define _RODEINT_H_

// Defining a few things will speed up inclusion of this file:
// RODEINT_TARGET_ONLY -- only includes target related files
// RODEINT_STEPPER_ONLY -- only includes stepper related files
// RODEINT_INTEGRATE_ONLY -- if this existed it would do that.
//   (But they depend on everything so are skipped)

// There are convenience includes
//   rodeint_target.h
//   rodeint_stepper.h
//   rodeint_integrate.h
// that can be used by external packages.

// NOTE: This file must be a ".h" file, not a ".hpp" file to work with
// Rcpp attributes.

// These files do not reference Rcpp, and so can be included first.
// Some of these are needed by rcpp_as_wrap_pre as they define types
// used within, saving the need for forward declaration.  They are not
// needed loading just the target.
#ifndef RODEINT_TARGET_ONLY
#include "stepper.hpp"
#endif

// This one goes first, because as/wrap prototypes must preceed
// Rcpp.h -- after this point anything is allowed to include Rcpp
// without causing a hassle.
#include "rcpp_as_wrap_pre.hpp"

#include <Rcpp.h>

// Then the actual code from the package:
#include "target_class.hpp"
#include "target_cpp.hpp"
#include "target_r.hpp"

// And then the actual as/wrap definitions now that all classes have
// been defined.
#include "rcpp_as_wrap_post.hpp"

#endif
