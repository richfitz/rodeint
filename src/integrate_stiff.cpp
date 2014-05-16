#include <rodeint/integrate_stiff.hpp>

#include <rodeint/ode_system_stiff_r.hpp>
#include <rodeint/ode_system_stiff_cpp.hpp>
#include <rodeint/ode_system_stiff_class.hpp>

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_stiff_${type}(rodeint::stepper s,
                        rodeint::ode_system_stiff_${type} system,
                        rodeint::ode_system_stiff_${type}::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const_stiff(s, system, y,
                                          t0, t1, dt, save_state);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_stiff_r(rodeint::stepper s,
                        rodeint::ode_system_stiff_r system,
                        rodeint::ode_system_stiff_r::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const_stiff(s, system, y,
                                          t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_stiff_cpp(rodeint::stepper s,
                        rodeint::ode_system_stiff_cpp system,
                        rodeint::ode_system_stiff_cpp::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const_stiff(s, system, y,
                                          t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_stiff_class(rodeint::stepper s,
                        rodeint::ode_system_stiff_class system,
                        rodeint::ode_system_stiff_class::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const_stiff(s, system, y,
                                          t0, t1, dt, save_state);
}
//[[[end]]]

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_stiff_${type}(rodeint::stepper s,
                          rodeint::ode_system_stiff_${type} system,
                          rodeint::ode_system_stiff_${type}::state_type y,
                          double t0, double dt, int n,
                          bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps_stiff(s, system, y,
                                            t0, dt, nu, save_state);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_stiff_r(rodeint::stepper s,
                          rodeint::ode_system_stiff_r system,
                          rodeint::ode_system_stiff_r::state_type y,
                          double t0, double dt, int n,
                          bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps_stiff(s, system, y,
                                            t0, dt, nu, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_stiff_cpp(rodeint::stepper s,
                          rodeint::ode_system_stiff_cpp system,
                          rodeint::ode_system_stiff_cpp::state_type y,
                          double t0, double dt, int n,
                          bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps_stiff(s, system, y,
                                            t0, dt, nu, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_stiff_class(rodeint::stepper s,
                          rodeint::ode_system_stiff_class system,
                          rodeint::ode_system_stiff_class::state_type y,
                          double t0, double dt, int n,
                          bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps_stiff(s, system, y,
                                            t0, dt, nu, save_state);
}
//[[[end]]]

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_${type}(rodeint::stepper s,
                           rodeint::ode_system_stiff_${type} system,
                           rodeint::ode_system_stiff_${type}::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(s, system, y,
                                             t0, t1, dt, save_state);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_r(rodeint::stepper s,
                           rodeint::ode_system_stiff_r system,
                           rodeint::ode_system_stiff_r::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(s, system, y,
                                             t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_cpp(rodeint::stepper s,
                           rodeint::ode_system_stiff_cpp system,
                           rodeint::ode_system_stiff_cpp::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(s, system, y,
                                             t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_class(rodeint::stepper s,
                           rodeint::ode_system_stiff_class system,
                           rodeint::ode_system_stiff_class::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(s, system, y,
                                             t0, t1, dt, save_state);
}
//[[[end]]]

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_stiff_${type}(rodeint::stepper s,
                        rodeint::ode_system_stiff_${type} system,
                        rodeint::ode_system_stiff_${type}::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times_stiff(s, system, y,
                                          times, dt);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_stiff_r(rodeint::stepper s,
                        rodeint::ode_system_stiff_r system,
                        rodeint::ode_system_stiff_r::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times_stiff(s, system, y,
                                          times, dt);
}
// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_stiff_cpp(rodeint::stepper s,
                        rodeint::ode_system_stiff_cpp system,
                        rodeint::ode_system_stiff_cpp::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times_stiff(s, system, y,
                                          times, dt);
}
// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_stiff_class(rodeint::stepper s,
                        rodeint::ode_system_stiff_class system,
                        rodeint::ode_system_stiff_class::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times_stiff(s, system, y,
                                          times, dt);
}
//[[[end]]]
