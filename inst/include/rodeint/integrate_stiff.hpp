#ifndef _RODEINT_INTEGRATE_STIFF_HPP_
#define _RODEINT_INTEGRATE_STIFF_HPP_

#ifdef ODEINT_INCLUDE_EVERYTHING
#include <boost/numeric/odeint.hpp>
#else
#include <boost/numeric/odeint/integrate/integrate_const.hpp>
#include <boost/numeric/odeint/integrate/integrate_n_steps.hpp>
#include <boost/numeric/odeint/integrate/integrate_adaptive.hpp>
#include <boost/numeric/odeint/integrate/integrate_times.hpp>
#include <boost/numeric/odeint/integrate/integrate.hpp>
#endif

#include <vector>
#include <Rcpp.h>

#include <rodeint/observers.hpp>
#include <rodeint/stepper.hpp>
#include <rodeint/util.hpp>

#include <rodeint/ode_system_stiff.hpp>

namespace rodeint {

// 1. integrate_const: "Equidistant observer calls"
template <typename OdeSystem>
struct integrate_const_stiff_data {
  typedef typename OdeSystem::state_type state_type;
  stepper s;
  OdeSystem system;
  state_type y;
  double t0, t1, dt;
  bool save_state;
  integrate_const_stiff_data(stepper s_, OdeSystem system_, state_type y_,
                             double t0_, double t1_, double dt_,
                             bool save_state_)
    : s(s_), system(system_),
      y(y_), t0(t0_), t1(t1_), dt(dt_), save_state(save_state_) {}

  template <typename Stepper>
  Rcpp::NumericVector run() {
    using boost::numeric::odeint::integrate_const;
    check_dt(t0, t1, dt);
    Stepper s_typed = s.template as<Stepper>(true);
    state_saver<state_type> state;
    // NOTE: This line is special to stiff systems.
    rodeint::ode_system_stiff_odeint<OdeSystem> sys(system);
    if (save_state) {
      state.steps =
        integrate_const(s_typed, sys, y, t0, t1, dt, state.obs);
    } else {
      integrate_const(s_typed, sys, y, t0, t1, dt);
    }
    return integration_state(y, state, save_state);
  }
  // And then set this up to work:
  /*[[[cog
    import generation as g
    g.integrate_stiff('Rcpp::NumericVector')
    ]]]*/
  // *** Generated section: do not edit until the end marker

  Rcpp::NumericVector run() {
    switch(s.category_id()) {
    case stepper::BASIC:
      return run_basic();
    case stepper::CONTROLLED:
      return run_controlled();
    default:
      stop("Unimplemented category"); // TODO: give details
      return Rcpp::NumericVector(); // never get here
    }
  }
  Rcpp::NumericVector run_basic() {
    switch(s.type_id()) {
    case stepper::EULER:
      return run<stepper_basic_euler_ublas>();
    case stepper::MODIFIED_MIDPOINT:
      return run<stepper_basic_modified_midpoint_ublas>();
    case stepper::RUNGE_KUTTA4:
      return run<stepper_basic_runge_kutta4_ublas>();
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_basic_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_basic_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_basic_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_basic_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericVector(); // never get here
  }
  Rcpp::NumericVector run_controlled() {
    switch(s.type_id()) {
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_controlled_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_controlled_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_controlled_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_controlled_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericVector(); // never get here
  }
  //[[[end]]]
};

// 2. integrate_n_steps: "Integrate a given number of steps"
template <typename OdeSystem>
struct integrate_n_steps_stiff_data {
  typedef typename OdeSystem::state_type state_type;
  stepper s;
  OdeSystem system;
  state_type y;
  double t0, dt;
  size_t n;
  bool save_state;
  integrate_n_steps_stiff_data(stepper s_, OdeSystem system_, state_type y_,
                               double t0_, double dt_, size_t n_,
                               bool save_state_)
    : s(s_), system(system_),
      y(y_), t0(t0_), dt(dt_), n(n_), save_state(save_state_) {}

  // This function actually unpacks the stepper to it's true type, and
  // organises running the integration, doing checks and organising
  // output.  All the interesting logic is kept here.
  template <typename Stepper>
  Rcpp::NumericVector run() {
    using boost::numeric::odeint::integrate_n_steps;
    // Different check on dt here, compared with the (t0, t1) integrators.
    if (dt == 0.0) {
      Rcpp::stop("dt cannot be zero");
    }
    Stepper s_typed = s.template as<Stepper>(true);
    state_saver<state_type> state;
    // NOTE: This line is special to stiff systems.
    rodeint::ode_system_stiff_odeint<OdeSystem> sys(system);
    if (save_state) {
      // NOTE: here, the final time is returned instead of n_steps
      integrate_n_steps(s_typed, sys, y, t0, dt, n, state.obs);
      state.steps = n;
    } else {
      integrate_n_steps(s_typed, sys, y, t0, dt, n);
    }
    return integration_state(y, state, save_state);
  }

  // And then set this up to work:
  /*[[[cog
    import generation as g
    g.integrate_stiff('Rcpp::NumericVector')
    ]]]*/
  // *** Generated section: do not edit until the end marker

  Rcpp::NumericVector run() {
    switch(s.category_id()) {
    case stepper::BASIC:
      return run_basic();
    case stepper::CONTROLLED:
      return run_controlled();
    default:
      stop("Unimplemented category"); // TODO: give details
      return Rcpp::NumericVector(); // never get here
    }
  }
  Rcpp::NumericVector run_basic() {
    switch(s.type_id()) {
    case stepper::EULER:
      return run<stepper_basic_euler_ublas>();
    case stepper::MODIFIED_MIDPOINT:
      return run<stepper_basic_modified_midpoint_ublas>();
    case stepper::RUNGE_KUTTA4:
      return run<stepper_basic_runge_kutta4_ublas>();
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_basic_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_basic_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_basic_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_basic_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericVector(); // never get here
  }
  Rcpp::NumericVector run_controlled() {
    switch(s.type_id()) {
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_controlled_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_controlled_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_controlled_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_controlled_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericVector(); // never get here
  }
  //[[[end]]]
};

// 3. integrate_adaptive "Observer calls at each step"
template <typename OdeSystem>
struct integrate_adaptive_stiff_data {
  typedef typename OdeSystem::state_type state_type;
  stepper s;
  OdeSystem system;
  state_type y;
  double t0, t1, dt;
  bool save_state;
  integrate_adaptive_stiff_data(stepper s_, OdeSystem system_, state_type y_,
                                double t0_, double t1_, double dt_,
                                bool save_state_)
    : s(s_), system(system_),
      y(y_), t0(t0_), t1(t1_), dt(dt_), save_state(save_state_) {}

  template <typename Stepper>
  Rcpp::NumericVector run() {
    using boost::numeric::odeint::integrate_adaptive;
    check_dt(t0, t1, dt);
    Stepper s_typed = s.template as<Stepper>(true);
    state_saver<state_type> state;
    // NOTE: This line is special to stiff systems.
    rodeint::ode_system_stiff_odeint<OdeSystem> sys(system);
    if (save_state) {
      state.steps =
        integrate_adaptive(s_typed, sys, y, t0, t1, dt, state.obs);
    } else {
      integrate_adaptive(s_typed, sys, y, t0, t1, dt);
    }
    return integration_state(y, state, save_state);
  }
  // And then set this up to work:
  /*[[[cog
    import generation as g
    g.integrate_stiff('Rcpp::NumericVector')
    ]]]*/
  // *** Generated section: do not edit until the end marker

  Rcpp::NumericVector run() {
    switch(s.category_id()) {
    case stepper::BASIC:
      return run_basic();
    case stepper::CONTROLLED:
      return run_controlled();
    default:
      stop("Unimplemented category"); // TODO: give details
      return Rcpp::NumericVector(); // never get here
    }
  }
  Rcpp::NumericVector run_basic() {
    switch(s.type_id()) {
    case stepper::EULER:
      return run<stepper_basic_euler_ublas>();
    case stepper::MODIFIED_MIDPOINT:
      return run<stepper_basic_modified_midpoint_ublas>();
    case stepper::RUNGE_KUTTA4:
      return run<stepper_basic_runge_kutta4_ublas>();
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_basic_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_basic_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_basic_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_basic_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericVector(); // never get here
  }
  Rcpp::NumericVector run_controlled() {
    switch(s.type_id()) {
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_controlled_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_controlled_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_controlled_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_controlled_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericVector(); // never get here
  }
  //[[[end]]]
};

// 4. integrate_times: "Observer calls at given time points"
//
// NOTE: In this case, state is *always* saved.
//
// NOTE: In this case the output format is different with the main
// returned thing being the matrix of times, and the final state 'y'
// now an attribute.
template <typename OdeSystem>
struct integrate_times_stiff_data {
  typedef typename OdeSystem::state_type state_type;
  stepper s;
  OdeSystem system;
  state_type y;
  std::vector<double> times;
  double dt;
  integrate_times_stiff_data(stepper s_, OdeSystem system_, state_type y_,
                             std::vector<double> times_, double dt_)
    : s(s_), system(system_),
      y(y_), times(times_), dt(dt_) {}

  // This function actually unpacks the stepper to it's true type, and
  // organises running the integration, doing checks and organising
  // output.  All the interesting logic is kept here.
  template <typename Stepper>
  Rcpp::NumericMatrix run() {
    using boost::numeric::odeint::integrate_times;

    // Quite different time testing to the other functions
    check_dt(times.front(), times.back(), dt);
    if (!util::is_sorted(times.begin(), times.end(), dt > 0)) {
      if (times.front() != times.back()) { // corner case :-/
        std::string msg = "Times must be sorted ";
        stop(msg + (dt > 0 ? "(increasing)" : "(decreasing)"));
      }
    }

    Stepper s_typed = s.template as<Stepper>(true);
    state_saver<state_type> state;
    // NOTE: This line is special to stiff systems.
    rodeint::ode_system_stiff_odeint<OdeSystem> sys(system);
    state.steps =
      integrate_times(s_typed, sys, y,
                      times.begin(), times.end(), dt, state.obs);

    Rcpp::NumericMatrix ret = util::to_rcpp_matrix_by_row(state.y);
    ret.attr("steps") = state.steps;
    ret.attr("t")     = state.t;
    ret.attr("y")     = Rcpp::NumericVector(y.begin(), y.end());
    return ret;
  }

  // And then set this up to work:
  /*[[[cog
    import generation as g
    g.integrate_stiff('Rcpp::NumericMatrix')
    ]]]*/
  // *** Generated section: do not edit until the end marker

  Rcpp::NumericMatrix run() {
    switch(s.category_id()) {
    case stepper::BASIC:
      return run_basic();
    case stepper::CONTROLLED:
      return run_controlled();
    default:
      stop("Unimplemented category"); // TODO: give details
      return Rcpp::NumericMatrix(); // never get here
    }
  }
  Rcpp::NumericMatrix run_basic() {
    switch(s.type_id()) {
    case stepper::EULER:
      return run<stepper_basic_euler_ublas>();
    case stepper::MODIFIED_MIDPOINT:
      return run<stepper_basic_modified_midpoint_ublas>();
    case stepper::RUNGE_KUTTA4:
      return run<stepper_basic_runge_kutta4_ublas>();
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_basic_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_basic_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_basic_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_basic_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericMatrix(); // never get here
  }
  Rcpp::NumericMatrix run_controlled() {
    switch(s.type_id()) {
    case stepper::RUNGE_KUTTA_CASH_KARP54:
      return run<stepper_controlled_runge_kutta_cash_karp54_ublas>();
    case stepper::RUNGE_KUTTA_FEHLBERG78:
      return run<stepper_controlled_runge_kutta_fehlberg78_ublas>();
    case stepper::RUNGE_KUTTA_DOPRI5:
      return run<stepper_controlled_runge_kutta_dopri5_ublas>();
    case stepper::ROSENBROCK4:
      return run<stepper_controlled_rosenbrock4_ublas>();
    default:
      stop("Unimplemented type"); // TODO: give details
    }
    return Rcpp::NumericMatrix(); // never get here
  }
  //[[[end]]]
};

// Functions we actually within integrate_stiff.cpp.  They're templated
// against the ode sytem (r, cpp, class).  All it does is collect the
// data object together and pass it along.
template <typename OdeSystem>
Rcpp::NumericVector
r_integrate_const_stiff(stepper s, OdeSystem system,
                        typename OdeSystem::state_type y,
                        double t0, double t1, double dt,
                        bool save_state) {
  integrate_const_stiff_data<OdeSystem> data(s, system, y,
                                             t0, t1, dt, save_state);
  return data.run();
}

template <typename OdeSystem>
Rcpp::NumericVector
r_integrate_n_steps_stiff(stepper s, OdeSystem system,
                          typename OdeSystem::state_type y,
                          double t0, double dt, size_t n,
                          bool save_state) {
  integrate_n_steps_stiff_data<OdeSystem> data(s, system, y,
                                               t0, dt, n, save_state);
  return data.run();
}

template <typename OdeSystem>
Rcpp::NumericVector
r_integrate_adaptive_stiff(stepper s, OdeSystem system,
                           typename OdeSystem::state_type y,
                           double t0, double t1, double dt,
                           bool save_state) {
  integrate_adaptive_stiff_data<OdeSystem> data(s, system, y,
                                                t0, t1, dt, save_state);
  return data.run();
}

// NOTE: (again) that the return type here is different to all other
// integrate functions.  save_state is always true, we always want 'y'
// at the intermediate times, so the primary output is the matrix of
// 'y' with the final state saved as an attribute "y".
template <typename OdeSystem>
Rcpp::NumericMatrix
r_integrate_times_stiff(stepper s, OdeSystem system,
                        typename OdeSystem::state_type y,
                        std::vector<double> times, double dt) {
  integrate_times_stiff_data<OdeSystem> data(s, system, y,
                                             times, dt);
  return data.run();
}


// NOTE: No integrate_simple (but that might go from the nonstiff
// cases anyway).

} // namespace

#endif
