#include "integrate.hpp"
#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"
#include "observers.hpp"
#include "controlled_stepper.hpp"
#include "util.hpp"

// TODO: Drop the default args here and in called functions.

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_simple(rodeint::target_r target,
                   rodeint::target_r::state_type y,
                   double t0, double t1, double dt,
                   bool save_state=false) {
  typedef rodeint::target_r::state_type state_type;
  using boost::numeric::odeint::integrate;

  rodeint::state_saver<state_type> state;

  if (save_state) {
    state.steps = integrate(target, y, t0, t1, dt, state.obs);
  } else {
    integrate(target, y, t0, t1, dt);
  }

  // This bit here duplicates code from rodeint::integration_state().
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (save_state) {
    state.add_state(ret);
  }

  return ret;
}

// In theory, non-controlled steppers could be used here.  We could
// generalise out even more of the hassle if boost variant would work
// in a nested way there (it might).

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_const(rodeint::controlled_stepper stepper,
                  rodeint::target_r target,
                  rodeint::target_r::state_type y,
                  double t0, double t1, double dt,
                  bool save_state) {
  rodeint::controlled_stepper_integrate_const
    vis(target, y, t0, t1, dt, save_state);
  boost::apply_visitor(vis, stepper);
  return vis.r_state();
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_n_steps(rodeint::controlled_stepper stepper,
                    rodeint::target_r target,
                    rodeint::target_r::state_type y,
                    double t0, double dt, size_t n,
                    bool save_state) {
  rodeint::controlled_stepper_integrate_n_steps
    vis(target, y, t0, dt, n, save_state);
  boost::apply_visitor(vis, stepper);
  return vis.r_state();
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_adaptive(rodeint::controlled_stepper stepper,
                     rodeint::target_r target,
                     rodeint::target_r::state_type y,
                     double t0, double t1, double dt,
                     bool save_state=false) {
  rodeint::controlled_stepper_integrate_adaptive
    vis(target, y, t0, t1, dt, save_state);
  boost::apply_visitor(vis, stepper);
  return vis.r_state();
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_times(rodeint::controlled_stepper stepper,
                  rodeint::target_r target,
                  rodeint::target_r::state_type y,
                  std::vector<double> times,
                  double dt) {
  rodeint::controlled_stepper_integrate_times
    vis(target, y, times.begin(), times.end(), dt);
  boost::apply_visitor(vis, stepper);
  return vis.r_state();
}
