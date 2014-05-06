#include "integrate.hpp"
#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"
#include "target_cpp.hpp"
#include "observers.hpp"
#include "stepper.hpp"
#include "util.hpp"

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_const_r(rodeint::stepper stepper,
                    rodeint::target_r target,
                    rodeint::target_r::state_type y,
                    double t0, double t1, double dt,
                    bool save_state) {
  return rodeint::r_integrate_const(stepper, target, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_const_cpp(rodeint::stepper stepper,
                      rodeint::target_cpp target,
                      rodeint::target_cpp::state_type y,
                      double t0, double t1, double dt,
                      bool save_state) {
  return rodeint::r_integrate_const(stepper, target, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_n_steps_r(rodeint::stepper stepper,
                      rodeint::target_r target,
                      rodeint::target_r::state_type y,
                      double t0, double dt, size_t n,
                      bool save_state) {
  return rodeint::r_integrate_n_steps(stepper, target, y,
                                      t0, dt, n, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_n_steps_cpp(rodeint::stepper stepper,
                        rodeint::target_cpp target,
                        rodeint::target_cpp::state_type y,
                        double t0, double dt, size_t n,
                        bool save_state) {
  return rodeint::r_integrate_n_steps(stepper, target, y,
                                      t0, dt, n, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_adaptive_r(rodeint::stepper stepper,
                       rodeint::target_r target,
                       rodeint::target_r::state_type y,
                       double t0, double t1, double dt,
                       bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, target, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_adaptive_cpp(rodeint::stepper stepper,
                         rodeint::target_cpp target,
                         rodeint::target_cpp::state_type y,
                         double t0, double t1, double dt,
                         bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, target, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_times_r(rodeint::stepper stepper,
                    rodeint::target_r target,
                    rodeint::target_r::state_type y,
                    std::vector<double> times,
                    double dt) {
  return rodeint::r_integrate_times(stepper, target, y,
                                    times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_times_cpp(rodeint::stepper stepper,
                      rodeint::target_cpp target,
                      rodeint::target_cpp::state_type y,
                      std::vector<double> times,
                      double dt) {
  return rodeint::r_integrate_times(stepper, target, y,
                                    times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_simple_r(rodeint::target_r target,
                     rodeint::target_r::state_type y,
                     double t0, double t1, double dt,
                     bool save_state) {
  return rodeint::r_integrate_simple(target, y,
                                     t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_simple_cpp(rodeint::target_cpp target,
                       rodeint::target_cpp::state_type y,
                       double t0, double t1, double dt,
                       bool save_state) {
  return rodeint::r_integrate_simple(target, y,
                                     t0, t1, dt, save_state);
}
