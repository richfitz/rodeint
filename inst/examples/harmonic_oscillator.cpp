// The next two lines tell Rcpp Attributes to set up the appropriate
// include paths, and to actually include the rodeint headers.
//
// Using the "smaller" header file "rodeint_target.h" that includes
// *only* the information needed to build the target is much faster
// than loading the full header file "rodeint.h"

// [[Rcpp::depends(rodeint)]]
#include <rodeint_target.h>

// #include <rodeint.h> // slower, but also OK

// This is the derivatives function -- it must have the signature
// below.  The 't' parameter name is commented out because it is not
// used (and so will generate a compiler warning), but the parameter
// is required.  This function can be within a namespace, etc.
//
// Because we're using namespaces here, it doesn't matter that the
// package already contains a 'harmonic_oscillator' function -- this
// one is safely in its own namespace, so duplicate symbols are
// avoided.
namespace examples {
void harmonic_oscillator(const std::vector<double>& y,
                         std::vector<double>& dydt,
                         const double /* t */,
                         const std::vector<double>& pars) {
  dydt[0] =  y[1];
  dydt[1] = -y[0] - pars[0] * y[1];
}
}

// This one here actually makes the target available to R via the Rcpp
// attributes.  As such, it must follow the rules set by Rcpp
// attributes (fully qualified return type, not in a namespace, etc).
//
// The 'pars' vector is initialised to the expected length -- that
// will be used to indicate the required length.

// [[Rcpp::export]]
rodeint::target_cpp example_harmonic_oscillator() {
  rodeint::target_cpp::pars_type pars(1); // establishes required length
  return rodeint::target_cpp(&examples::harmonic_oscillator, pars);
}
