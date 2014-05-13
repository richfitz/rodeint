#include <rodeint/ode_system_stiff_r.hpp>
#include <rodeint/ode_system_stiff_cpp.hpp>
#include <rodeint/ode_system_stiff_class.hpp>

// General arrangement of types
#include <rodeint/ode_system_stiff.hpp>

// Need to trim this down!
#include <boost/numeric/odeint.hpp>

// Stepper information, may move to a stiff file.
#include <rodeint/stepper.hpp>

#include <rodeint/observers.hpp>

// TODO: Everything.  This is a prototype, no steppers passed in, not
// dealing with observers, one integrate function only.  Baby steps.

namespace rodeint {

template <typename OdeSystem>
Rcpp::NumericVector
r_integrate_adaptive_stiff(stepper_stiff /* stepper*/,
                           OdeSystem ode_system,
                           typename OdeSystem::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  check_dt(t0, t1, dt);

  state_saver<typename OdeSystem::state_type> state;

  using boost::numeric::odeint::integrate_adaptive;
  using boost::numeric::odeint::make_dense_output;
  using boost::numeric::odeint::rosenbrock4;
  stepper_dense_rosenbrock4 stepper =
    make_dense_output<rosenbrock4<double > >(1.0e-6 ,1.0e-6);
  ode_system_stiff_odeint<OdeSystem> sys(ode_system);

  if (save_state) {
    state.steps =
      integrate_adaptive(stepper, sys, y, t0, t1, dt, state.obs);
  } else {
    integrate_adaptive(stepper, sys, y, t0, t1, dt);
  }
  // This section is done through the cleanup:
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (save_state) {
    ret.attr("steps") = state.steps;
    ret.attr("t")     = state.t;
    ret.attr("y")     = util::to_rcpp_matrix_by_row(state.y);
  }
  return ret;
}

}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_r(rodeint::stepper_stiff stepper,
                           rodeint::ode_system_stiff_r ode_system,
                           rodeint::ode_system_stiff_r::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(stepper, ode_system,
                                             y, t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_cpp(rodeint::stepper_stiff stepper,
                             rodeint::ode_system_stiff_cpp ode_system,
                             rodeint::ode_system_stiff_cpp::state_type y,
                             double t0, double t1, double dt,
                             bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(stepper, ode_system,
                                             y, t0, t1, dt, save_state);
}

// [[Rcpp::export]]
Rcpp::NumericVector
integrate_adaptive_stiff_class(rodeint::stepper_stiff stepper,
                               rodeint::ode_system_stiff_class ode_system,
                               rodeint::ode_system_stiff_class::state_type y,
                               double t0, double t1, double dt,
                               bool save_state) {
  return rodeint::r_integrate_adaptive_stiff(stepper, ode_system,
                                             y, t0, t1, dt, save_state);
}
