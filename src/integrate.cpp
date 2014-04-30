#include "integrate.hpp"

// [[Rcpp::export]]
rodeint::integrator integrator_make(rodeint::ode_target_r target) {
  return rodeint::integrator(target);
}

// [[Rcpp::export]]
rodeint::integrator::state_type
integrator_derivs(Rcpp::XPtr<rodeint::integrator> obj,
                  const rodeint::integrator::state_type& y,
                  const double t) {
  return obj->derivs(y, t);
}

// [[Rcpp::export]]
rodeint::integrator::state_type
integrator_integrate(Rcpp::XPtr<rodeint::integrator> obj,
                     rodeint::integrator::state_type y,
                     double t0, double t1, double dt) {
  return obj->integrate(y, t0, t1, dt);
}
