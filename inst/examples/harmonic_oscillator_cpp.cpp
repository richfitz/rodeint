// Run this file with
//   Rcpp::sourceCpp("harmonic_oscillator_cpp.cpp")
// or
//   Rcpp::sourceCpp("harmonic_oscillator_cpp.cpp", embeddedR=FALSE)


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
// Can do parameter checking here -- in particular the size of the
// first-given parameter set is assumed always to be the correct size,
// so validate it here.

// [[Rcpp::export]]
rodeint::target_cpp example_harmonic_oscillator_cpp(std::vector<double> pars) {
  rodeint::util::check_length(pars.size(), 1);
  return rodeint::target_cpp(&examples::harmonic_oscillator, pars);
}

/*** R
    ## Run this file with
    ##   Rcpp::sourceCpp("harmonic_oscillator_cpp.cpp")
    ## or
    ##   Rcpp::sourceCpp("harmonic_oscillator_cpp.cpp", embeddedR=FALSE)
    library(rodeint)

    obj <- target_cpp(example_harmonic_oscillator_cpp, 0.5)
    f <- make_integrate(obj, t0=0, dt=0.01)
    y <- c(0, 1)

    ## Evaluate the function at some times in the future by numerically
    ## solving the differential equations:
    f(y, 1)
    f(y, 10)

    ## Collect information on where the objective function was evaluated:
    res <- f(c(0, 1), 10, TRUE)
    matplot(attr(res, "t"), attr(res, "y"), type="l",
            xlab="Time", ylab="Variables")
 */
