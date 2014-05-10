// -*-c++-*-
#ifndef _RODEINT_H_
#define _RODEINT_H_

// Defining
//   RODEINT_ODE_SYSTEM_ONLY
// will speed up inclusion of this file by only including ode_system
// related files.  This is best when making new functions.  Or just
// include "rodeint_ode_system.h" which does it for you.
//
// Conversely defining
//   ODEINT_INCLUDE_EVERYTHING
// will slow compilation down.

// NOTE: This file must be a ".h" file, not a ".hpp" file to work with
// Rcpp attributes.

// These files do not reference Rcpp, and so can be included first.
// Some of these are needed by rcpp_pre as they define types
// used within, saving the need for forward declaration.  They are not
// needed loading just the ode_system.
#ifndef RODEINT_ODE_SYSTEM_ONLY
#include <rodeint/stepper.hpp>
#endif

// This one goes first, because as/wrap prototypes must preceed
// Rcpp.h -- after this point anything is allowed to include Rcpp
// without causing a hassle.
#include <rodeint/rcpp_pre.hpp>

#include <Rcpp.h>

// Then the actual code from the package:
#include <rodeint/ode_system_class.hpp>
#include <rodeint/ode_system_cpp.hpp>
#include <rodeint/ode_system_r.hpp>

// And then the actual as/wrap definitions now that all classes have
// been defined.
#include <rodeint/rcpp_post.hpp>

#endif
