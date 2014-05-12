// Run this file with
//   Rcpp::sourceCpp("harmonic_oscillator_class.cpp")
// or
//   Rcpp::sourceCpp("harmonic_oscillator_cpp.cpp", embeddedR=FALSE)

// [[Rcpp::depends(rodeint)]]
#include <rodeint_ode_system.h>

namespace examples {

class harmonic_oscillator {
public:
  typedef double pars_type;
  harmonic_oscillator(double p_) : p(p_) {}
  void derivs(const std::vector<double>& y, std::vector<double>& dydt,
              const double /* t */) { // not const
    dydt[0] =  y[1];
    dydt[1] = -y[0] - p * y[1];
  }
  void set_pars(SEXP pars) {
    p = Rcpp::as<double>(pars);
  }
private:
  double p;
};

}

// [[Rcpp::export]]
rodeint::ode_system_class
example_harmonic_oscillator_class(double pars) {
  using examples::harmonic_oscillator;
  return rodeint::make_ode_system_class<harmonic_oscillator>(pars);
}

/*** R
    ## Run this file with
    ##   Rcpp::sourceCpp("harmonic_oscillator_class.cpp")
    library(rodeint)

    obj <- ode_system(example_harmonic_oscillator_class, 0.5)
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
