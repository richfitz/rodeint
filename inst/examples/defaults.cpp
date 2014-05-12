// Run with
//   Rcpp::sourceCpp("defaults.cpp")
// but this one is just for testing

// [[Rcpp::depends(rodeint)]]
#include <rodeint_ode_system.h>

namespace examples {

class system_default_methods {
public:
  typedef double pars_type;
  system_default_methods(double p_) : p(p_) {}
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

class system_other_methods {
public:
  typedef double pars_type;
  system_other_methods(double p_) : p(p_) {}
  void derivatives(const std::vector<double>& y, std::vector<double>& dydt,
                   const double /* t */) { // not const
    dydt[0] =  y[1];
    dydt[1] = -y[0] - p * y[1];
  }
  void set_parameters(SEXP pars) {
    p = Rcpp::as<double>(pars);
  }
private:
  double p;
};

}

// [[Rcpp::export]]
rodeint::ode_system_class
example_system_default(double pars) {
  typedef examples::system_default_methods sys;
  return rodeint::make_ode_system_class<sys>(pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class
example_system_specified(double pars) {
  typedef examples::system_default_methods sys;
  return rodeint::make_ode_system_class<sys,
                                        &sys::derivs,
                                        &sys::set_pars>(pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class
example_system_other(double pars) {
  typedef examples::system_other_methods sys;
  return rodeint::make_ode_system_class<sys,
                                        &sys::derivatives,
                                        &sys::set_parameters>(pars);
}

// Second type of constructor:

// [[Rcpp::export]]
rodeint::ode_system_class
example_system_default2(double pars) {
  typedef examples::system_default_methods sys;
  sys obj(pars);
  return rodeint::make_ode_system_class<sys>(obj, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class
example_system_specified2(double pars) {
  typedef examples::system_default_methods sys;
  sys obj(pars);
  return rodeint::make_ode_system_class<sys,
                                        &sys::derivs,
                                        &sys::set_pars>(obj, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class
example_system_other2(double pars) {
  typedef examples::system_other_methods sys;
  sys obj(pars);
  return rodeint::make_ode_system_class<sys,
                                        &sys::derivatives,
                                        &sys::set_parameters>(obj, pars);
}

/*** R
    ## Run this file with
    ##   Rcpp::sourceCpp("defaults.cpp")
    ## but this one is just for testing :)
    library(rodeint)
    library(testthat)

    pars <- 0.5
    system_default    <- ode_system(example_system_default, pars)
    system_specified  <- ode_system(example_system_specified, pars)
    system_other      <- ode_system(example_system_other, pars)
    system_default2   <- ode_system(example_system_default2, pars)
    system_specified2 <- ode_system(example_system_specified2, pars)
    system_other2     <- ode_system(example_system_other2, pars)

    expect_that(system_default$get_pars(),    is_identical_to(pars))
    expect_that(system_specified$get_pars(),  is_identical_to(pars))
    expect_that(system_other$get_pars(),      is_identical_to(pars))
    expect_that(system_default2$get_pars(),   is_identical_to(pars))
    expect_that(system_specified2$get_pars(), is_identical_to(pars))
    expect_that(system_other2$get_pars(),     is_identical_to(pars))

    y <- c(0, 1)
    t <- 0
    ans <- c(1.0, -0.5)
    expect_that(system_default$derivs(y, t),    is_identical_to(ans))
    expect_that(system_specified$derivs(y, t),  is_identical_to(ans))
    expect_that(system_other$derivs(y, t),      is_identical_to(ans))
    expect_that(system_default2$derivs(y, t),   is_identical_to(ans))
    expect_that(system_specified2$derivs(y, t), is_identical_to(ans))
    expect_that(system_other2$derivs(y, t),     is_identical_to(ans))

    pars2 <- 0.25
    system_default$set_pars(pars2)
    system_specified$set_pars(pars2)
    system_other$set_pars(pars2)
    system_default2$set_pars(pars2)
    system_specified2$set_pars(pars2)
    system_other2$set_pars(pars2)

    plt <- function(sys) {
      res <- make_integrate(sys, t0=0, dt=0.01)(y, 10, TRUE)
      matplot(attr(res, "t"), attr(res, "y"), type="l",
              xlab="Time", ylab="Variables")
    }

    plt(system_default)
    plt(system_specified)
    plt(system_other)
    plt(system_default2)
    plt(system_specified2)
    plt(system_other2)

    if (!interactive()) {
       Sys.sleep(2)
    }
 */
