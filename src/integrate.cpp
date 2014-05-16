#include <rodeint/integrate.hpp>

#include <rodeint/ode_system_r.hpp>
#include <rodeint/ode_system_cpp.hpp>
#include <rodeint/ode_system_class.hpp>

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_${type}(rodeint::stepper s,
                        rodeint::ode_system_${type} ode_system,
                        rodeint::ode_system_${type}::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const(s, ode_system, y,
                                    t0, t1, dt, save_state);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_r(rodeint::stepper s,
                        rodeint::ode_system_r ode_system,
                        rodeint::ode_system_r::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const(s, ode_system, y,
                                    t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_cpp(rodeint::stepper s,
                        rodeint::ode_system_cpp ode_system,
                        rodeint::ode_system_cpp::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const(s, ode_system, y,
                                    t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_const_class(rodeint::stepper s,
                        rodeint::ode_system_class ode_system,
                        rodeint::ode_system_class::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  return rodeint::r_integrate_const(s, ode_system, y,
                                    t0, t1, dt, save_state);
}
//[[[end]]]

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_${type}(rodeint::stepper s,
                    rodeint::ode_system_${type} ode_system,
                    rodeint::ode_system_${type}::state_type y,
                    double t0, double dt, int n,
                    bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(s, ode_system, y,
                                      t0, dt, nu, save_state);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_r(rodeint::stepper s,
                    rodeint::ode_system_r ode_system,
                    rodeint::ode_system_r::state_type y,
                    double t0, double dt, int n,
                    bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(s, ode_system, y,
                                      t0, dt, nu, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_cpp(rodeint::stepper s,
                    rodeint::ode_system_cpp ode_system,
                    rodeint::ode_system_cpp::state_type y,
                    double t0, double dt, int n,
                    bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(s, ode_system, y,
                                      t0, dt, nu, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_n_steps_class(rodeint::stepper s,
                    rodeint::ode_system_class ode_system,
                    rodeint::ode_system_class::state_type y,
                    double t0, double dt, int n,
                    bool save_state) {
  size_t nu = rodeint::util::safe_size_t_from_r(n);
  return rodeint::r_integrate_n_steps(s, ode_system, y,
                                      t0, dt, nu, save_state);
}
//[[[end]]]

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_${type}(rodeint::stepper s,
                           rodeint::ode_system_${type} ode_system,
                           rodeint::ode_system_${type}::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive(s, ode_system, y,
                                       t0, t1, dt, save_state);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_r(rodeint::stepper s,
                           rodeint::ode_system_r ode_system,
                           rodeint::ode_system_r::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive(s, ode_system, y,
                                       t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_cpp(rodeint::stepper s,
                           rodeint::ode_system_cpp ode_system,
                           rodeint::ode_system_cpp::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive(s, ode_system, y,
                                       t0, t1, dt, save_state);
}
// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_class(rodeint::stepper s,
                           rodeint::ode_system_class ode_system,
                           rodeint::ode_system_class::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive(s, ode_system, y,
                                       t0, t1, dt, save_state);
}
//[[[end]]]

/*[[[cog
import cog
from generation import system_types, tp
template = """// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_${type}(rodeint::stepper s,
                        rodeint::ode_system_${type} ode_system,
                        rodeint::ode_system_${type}::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times(s, ode_system, y,
                                    times, dt);
}
"""
for t in system_types:
  cog.out(tp(template, {'type': t}))
]]]*/
// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_r(rodeint::stepper s,
                        rodeint::ode_system_r ode_system,
                        rodeint::ode_system_r::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times(s, ode_system, y,
                                    times, dt);
}
// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_cpp(rodeint::stepper s,
                        rodeint::ode_system_cpp ode_system,
                        rodeint::ode_system_cpp::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times(s, ode_system, y,
                                    times, dt);
}
// [[Rcpp::export]]
Rcpp::NumericMatrix
integrate_times_class(rodeint::stepper s,
                        rodeint::ode_system_class ode_system,
                        rodeint::ode_system_class::state_type y,
                        std::vector<double> times, double dt) {
  return rodeint::r_integrate_times(s, ode_system, y,
                                    times, dt);
}
//[[[end]]]
