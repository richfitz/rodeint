#include <rodeint/integrate.hpp>

#include <rodeint/target_r.hpp>
#include <rodeint/target_cpp.hpp>
#include <rodeint/target_class.hpp>

// TODO: This file would be substantially easier to look after with
// code generation -- at least for the different target types, but
// possibly also for the different intgration types, which differ in
// easy to catalogue ways.

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_r(rodeint::stepper stepper,
                  rodeint::target_r target,
                  rodeint::target_r::state_type y,
                  double t0, double t1, double dt,
                  bool save_state) {
  return rodeint::r_integrate_const(stepper, target, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_cpp(rodeint::stepper stepper,
                    rodeint::target_cpp target,
                    rodeint::target_cpp::state_type y,
                    double t0, double t1, double dt,
                    bool save_state) {
  return rodeint::r_integrate_const(stepper, target, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_class(rodeint::stepper stepper,
                      rodeint::target_class target,
                      rodeint::target_class::state_type y,
                      double t0, double t1, double dt,
                      bool save_state) {
  return rodeint::r_integrate_const(stepper, target, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_r(rodeint::stepper stepper,
                    rodeint::target_r target,
                    rodeint::target_r::state_type y,
                    double t0, double dt, int n,
                    bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(stepper, target, y,
                                      t0, dt, nu, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_cpp(rodeint::stepper stepper,
                      rodeint::target_cpp target,
                      rodeint::target_cpp::state_type y,
                      double t0, double dt, int n,
                      bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  // Rprintf("n: int = %d, unsigned long = %lu, size_t = %zu\n", n, n, n);
  return rodeint::r_integrate_n_steps(stepper, target, y,
                                      t0, dt, nu, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_class(rodeint::stepper stepper,
                        rodeint::target_class target,
                        rodeint::target_class::state_type y,
                        double t0, double dt, int n,
                        bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(stepper, target, y,
                                      t0, dt, nu, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_r(rodeint::stepper stepper,
                     rodeint::target_r target,
                     rodeint::target_r::state_type y,
                     double t0, double t1, double dt,
                     bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, target, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_cpp(rodeint::stepper stepper,
                       rodeint::target_cpp target,
                       rodeint::target_cpp::state_type y,
                       double t0, double t1, double dt,
                       bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, target, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_class(rodeint::stepper stepper,
                         rodeint::target_class target,
                         rodeint::target_class::state_type y,
                         double t0, double t1, double dt,
                         bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, target, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_r(rodeint::stepper stepper,
                  rodeint::target_r target,
                  rodeint::target_r::state_type y,
                  std::vector<double> times,
                  double dt) {
  return rodeint::r_integrate_times(stepper, target, y, times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_times_cpp(rodeint::stepper stepper,
                    rodeint::target_cpp target,
                    rodeint::target_cpp::state_type y,
                    std::vector<double> times,
                    double dt) {
  return rodeint::r_integrate_times(stepper, target, y,
                                    times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_times_class(rodeint::stepper stepper,
                      rodeint::target_class target,
                      rodeint::target_class::state_type y,
                      std::vector<double> times,
                      double dt) {
  return rodeint::r_integrate_times(stepper, target, y,
                                    times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_simple_r(rodeint::target_r target,
                   rodeint::target_r::state_type y,
                   double t0, double t1, double dt,
                   bool save_state) {
  return rodeint::r_integrate_simple(target, y,
                                     t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_simple_cpp(rodeint::target_cpp target,
                     rodeint::target_cpp::state_type y,
                     double t0, double t1, double dt,
                     bool save_state) {
  return rodeint::r_integrate_simple(target, y,
                                     t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_simple_class(rodeint::target_class target,
                       rodeint::target_class::state_type y,
                       double t0, double t1, double dt,
                       bool save_state) {
  return rodeint::r_integrate_simple(target, y,
                                     t0, t1, dt, save_state);
}
