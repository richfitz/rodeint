#include <rodeint/integrate.hpp>

#include <rodeint/ode_system_r.hpp>
#include <rodeint/ode_system_cpp.hpp>
#include <rodeint/ode_system_class.hpp>

// TODO: This file would be substantially easier to look after with
// code generation -- at least for the different ode_system types, but
// possibly also for the different intgration types, which differ in
// easy to catalogue ways.

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_r(rodeint::stepper stepper,
                  rodeint::ode_system_r ode_system,
                  rodeint::ode_system_r::state_type y,
                  double t0, double t1, double dt,
                  bool save_state) {
  return rodeint::r_integrate_const(stepper, ode_system, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_cpp(rodeint::stepper stepper,
                    rodeint::ode_system_cpp ode_system,
                    rodeint::ode_system_cpp::state_type y,
                    double t0, double t1, double dt,
                    bool save_state) {
  return rodeint::r_integrate_const(stepper, ode_system, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_class(rodeint::stepper stepper,
                      rodeint::ode_system_class ode_system,
                      rodeint::ode_system_class::state_type y,
                      double t0, double t1, double dt,
                      bool save_state) {
  return rodeint::r_integrate_const(stepper, ode_system, y,
                                    t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_r(rodeint::stepper stepper,
                    rodeint::ode_system_r ode_system,
                    rodeint::ode_system_r::state_type y,
                    double t0, double dt, int n,
                    bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(stepper, ode_system, y,
                                      t0, dt, nu, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_cpp(rodeint::stepper stepper,
                      rodeint::ode_system_cpp ode_system,
                      rodeint::ode_system_cpp::state_type y,
                      double t0, double dt, int n,
                      bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  // Rprintf("n: int = %d, unsigned long = %lu, size_t = %zu\n", n, n, n);
  return rodeint::r_integrate_n_steps(stepper, ode_system, y,
                                      t0, dt, nu, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_class(rodeint::stepper stepper,
                        rodeint::ode_system_class ode_system,
                        rodeint::ode_system_class::state_type y,
                        double t0, double dt, int n,
                        bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(stepper, ode_system, y,
                                      t0, dt, nu, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_r(rodeint::stepper stepper,
                     rodeint::ode_system_r ode_system,
                     rodeint::ode_system_r::state_type y,
                     double t0, double t1, double dt,
                     bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, ode_system, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_cpp(rodeint::stepper stepper,
                       rodeint::ode_system_cpp ode_system,
                       rodeint::ode_system_cpp::state_type y,
                       double t0, double t1, double dt,
                       bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, ode_system, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_class(rodeint::stepper stepper,
                         rodeint::ode_system_class ode_system,
                         rodeint::ode_system_class::state_type y,
                         double t0, double t1, double dt,
                         bool save_state=false) {
  return rodeint::r_integrate_adaptive(stepper, ode_system, y,
                                       t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_r(rodeint::stepper stepper,
                  rodeint::ode_system_r ode_system,
                  rodeint::ode_system_r::state_type y,
                  std::vector<double> times,
                  double dt) {
  return rodeint::r_integrate_times(stepper, ode_system, y, times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_times_cpp(rodeint::stepper stepper,
                    rodeint::ode_system_cpp ode_system,
                    rodeint::ode_system_cpp::state_type y,
                    std::vector<double> times,
                    double dt) {
  return rodeint::r_integrate_times(stepper, ode_system, y,
                                    times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_times_class(rodeint::stepper stepper,
                      rodeint::ode_system_class ode_system,
                      rodeint::ode_system_class::state_type y,
                      std::vector<double> times,
                      double dt) {
  return rodeint::r_integrate_times(stepper, ode_system, y,
                                    times, dt);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_simple_r(rodeint::ode_system_r ode_system,
                   rodeint::ode_system_r::state_type y,
                   double t0, double t1, double dt,
                   bool save_state) {
  return rodeint::r_integrate_simple(ode_system, y,
                                     t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_simple_cpp(rodeint::ode_system_cpp ode_system,
                     rodeint::ode_system_cpp::state_type y,
                     double t0, double t1, double dt,
                     bool save_state) {
  return rodeint::r_integrate_simple(ode_system, y,
                                     t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_simple_class(rodeint::ode_system_class ode_system,
                       rodeint::ode_system_class::state_type y,
                       double t0, double t1, double dt,
                       bool save_state) {
  return rodeint::r_integrate_simple(ode_system, y,
                                     t0, t1, dt, save_state);
}
