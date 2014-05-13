#include <rodeint/ode_system_stiff_r.hpp>
#include <rodeint/ode_system_stiff_cpp.hpp>
#include <rodeint/ode_system_stiff_class.hpp>

// General arrangement of types
#include <rodeint/ode_system_stiff.hpp>

// Need to trim this down!
#include <boost/numeric/odeint.hpp>

// Stepper information, may move to a stiff file.
#include <rodeint/stepper.hpp>

// TODO: Everything.  This is a prototype, no steppers passed in, not
// dealing with observers, one integrate function only.  Baby steps.

namespace rodeint {

template <typename OdeSystem>
Rcpp::NumericVector
r_integrate_stiff_adaptive(/* stepper_stiff stepper,*/
                           OdeSystem ode_system,
                           typename OdeSystem::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  check_dt(t0, t1, dt);

  using boost::numeric::odeint::integrate_adaptive;
  using boost::numeric::odeint::make_dense_output;
  using boost::numeric::odeint::rosenbrock4;
  stepper_dense_rosenbrock4 stepper =
    make_dense_output<rosenbrock4<double > >(1.0e-6 ,1.0e-6);
  ode_system_stiff_odeint<OdeSystem> sys(ode_system);
  size_t n_steps = integrate_adaptive(stepper, sys, y, t0, t1, dt);
  // This section is done through the cleanup:
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (save_state) {
    ret.attr("steps") = n_steps;
  }
  return ret;
}

}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_stiff_adaptive_r(rodeint::ode_system_stiff_r ode_system,
                             rodeint::ode_system_stiff_r::state_type y,
                             double t0, double t1, double dt,
                             bool save_state) {
  return rodeint::r_integrate_stiff_adaptive(ode_system, y, t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_stiff_adaptive_cpp(rodeint::ode_system_stiff_cpp ode_system,
                               rodeint::ode_system_stiff_cpp::state_type y,
                               double t0, double t1, double dt,
                               bool save_state) {
  return rodeint::r_integrate_stiff_adaptive(ode_system, y, t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
r_integrate_stiff_adaptive_class(rodeint::ode_system_stiff_class ode_system,
                                 rodeint::ode_system_stiff_class::state_type y,
                                 double t0, double t1, double dt,
                                 bool save_state) {
  return rodeint::r_integrate_stiff_adaptive(ode_system, y, t0, t1, dt, save_state);
}
