#ifndef _RODEINT_INTEGRATE_HPP_
#define _RODEINT_INTEGRATE_HPP_

#include <boost/numeric/odeint.hpp>
#include <boost/variant.hpp>
#include <vector>
#include <Rcpp.h>
#include "observers.hpp"
#include "stepper.hpp"
#include "util.hpp"

namespace rodeint {

// This is a helper used by many of the functions below, which saves
// the state in a repeatable way and reduces repetition.
template <typename State>
Rcpp::NumericVector integration_state(const std::vector<double>& y,
                                      const State& state,
                                      bool save_state) {
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (save_state) {
    ret.attr("steps") = state.steps;
    ret.attr("t")     = state.t;
    ret.attr("y")     = util::to_rcpp_matrix_by_row(state.y);
  }
  return ret;
}

// There is quite a bit of repetition here, but it's not *that* bad.
// This gives us compile time polymorphism based on runtime input
// though, which is pretty sweet.  Eventually most of the repetition
// can be removed by code generation.

// 1: integrate_const: "Equidistant observer calls"
template <typename Target>
class stepper_integrate_const : boost::static_visitor<> {
public:
  typedef Target target_type;
  typedef typename Target::state_type state_type;
  Target target;
  state_type& y;
  double t0, t1, dt;
  bool save_state;
  state_saver<state_type> state;

  typedef void result_type;
  stepper_integrate_const(Target target_, state_type &y_,
                          double t0_, double t1_, double dt_,
                          bool save_state_)
    : target(target_), y(y_), t0(t0_), t1(t1_), dt(dt_),
      save_state(save_state_) {}
  void operator()(stepper_basic_euler s) {
    integrate_const(s);
  }
  void operator()(stepper_basic_modified_midpoint s) {
    integrate_const(s);
  }
  void operator()(stepper_basic_runge_kutta4 s) {
    integrate_const(s);
  }
  void operator()(stepper_basic_runge_kutta_cash_karp54 s) {
    integrate_const(s);
  }
  void operator()(stepper_basic_runge_kutta_fehlberg78 s) {
    integrate_const(s);
  }
  void operator()(stepper_basic_runge_kutta_dopri5 s) {
    integrate_const(s);
  }

  void operator()(stepper_controlled_runge_kutta_cash_karp54 s) {
    integrate_const(s);
  }
  void operator()(stepper_controlled_runge_kutta_fehlberg78 s) {
    integrate_const(s);
  }
  void operator()(stepper_controlled_runge_kutta_dopri5 s) {
    integrate_const(s);
  }
private:
  template <typename Stepper>
  void integrate_const(Stepper s) {
    using boost::numeric::odeint::integrate_const;
    if (save_state) {
      state.steps =
        integrate_const(s, target, y, t0, t1, dt, state.obs);
    } else {
      integrate_const(s, target, y, t0, t1, dt);
    }
  }
};

template <typename Target>
Rcpp::NumericVector
r_integrate_const(stepper stepper, Target target,
                  typename Target::state_type y,
                  double t0, double t1, double dt, bool save_state) {
  check_dt(t0, t1, dt);
  stepper_integrate_const<Target>
    vis(target, y, t0, t1, dt, save_state);
  boost::apply_visitor(vis, stepper);
  return integration_state(y, vis.state, save_state);
}

// 2. integrate_n_steps: "Integrate a given number of steps"
template <typename Target>
class stepper_integrate_n_steps : boost::static_visitor<> {
public:
  typedef Target target_type;
  typedef typename Target::state_type state_type;
  Target target;
  state_type& y;
  double t0, dt;
  size_t n;
  bool save_state;
  state_saver<state_type> state;

  typedef void result_type;
  stepper_integrate_n_steps(Target target_, state_type &y_,
                            double t0_, double dt_, size_t n_,
                            bool save_state_)
    : target(target_), y(y_), t0(t0_), dt(dt_), n(n_),
      save_state(save_state_) {}
  void operator()(stepper_basic_euler s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_basic_modified_midpoint s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_basic_runge_kutta4 s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_basic_runge_kutta_cash_karp54 s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_basic_runge_kutta_fehlberg78 s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_basic_runge_kutta_dopri5 s) {
    integrate_n_steps(s);
  }

  void operator()(stepper_controlled_runge_kutta_cash_karp54 s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_controlled_runge_kutta_fehlberg78 s) {
    integrate_n_steps(s);
  }
  void operator()(stepper_controlled_runge_kutta_dopri5 s) {
    integrate_n_steps(s);
  }
private:
  template <typename Stepper>
  void integrate_n_steps(Stepper s) {
    using boost::numeric::odeint::integrate_n_steps;
    if (save_state) {
      // NOTE: here, the final time is returned instead of n_steps
      integrate_n_steps(s, target, y, t0, dt, n, state.obs);
      state.steps = n;
    } else {
      integrate_n_steps(s, target, y, t0, dt, n);
    }
  }
};

template <typename Target>
Rcpp::NumericVector
r_integrate_n_steps(stepper stepper, Target target,
                    typename Target::state_type y,
                    double t0, double dt, size_t n, bool save_state) {
  // Different check on dt here, compared with the (t0, t1) integrators.
  if (dt == 0.0) {
    Rcpp::stop("dt cannot be zero");
  }
  stepper_integrate_n_steps<Target>
    vis(target, y, t0, dt, n, save_state);
  boost::apply_visitor(vis, stepper);
  return integration_state(y, vis.state, save_state);
}

// 3. integrate_adaptive "Observer calls at each step"
template <typename Target>
class stepper_integrate_adaptive : boost::static_visitor<> {
public:
  typedef Target target_type;
  typedef typename Target::state_type state_type;
  Target target;
  state_type& y;
  double t0, t1, dt;
  bool save_state;
  state_saver<state_type> state;

  typedef void result_type;
  stepper_integrate_adaptive(Target target_, state_type &y_,
                             double t0_, double t1_, double dt_,
                             bool save_state_)
    : target(target_), y(y_), t0(t0_), t1(t1_), dt(dt_),
      save_state(save_state_) {}
  void operator()(stepper_basic_euler s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_basic_modified_midpoint s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_basic_runge_kutta4 s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_basic_runge_kutta_cash_karp54 s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_basic_runge_kutta_fehlberg78 s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_basic_runge_kutta_dopri5 s) {
    integrate_adaptive(s);
  }

  void operator()(stepper_controlled_runge_kutta_cash_karp54 s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_controlled_runge_kutta_fehlberg78 s) {
    integrate_adaptive(s);
  }
  void operator()(stepper_controlled_runge_kutta_dopri5 s) {
    integrate_adaptive(s);
  }
private:
  template <typename Stepper>
  void integrate_adaptive(Stepper s) {
    using boost::numeric::odeint::integrate_adaptive;
    if (save_state) {
      state.steps =
        integrate_adaptive(s, target, y, t0, t1, dt, state.obs);
    } else {
      integrate_adaptive(s, target, y, t0, t1, dt);
    }
  }
};

template <typename Target>
Rcpp::NumericVector
r_integrate_adaptive(stepper stepper, Target target,
                     typename Target::state_type y,
                     double t0, double t1, double dt, bool save_state) {
  check_dt(t0, t1, dt);
  stepper_integrate_adaptive<Target>
    vis(target, y, t0, t1, dt, save_state);
  boost::apply_visitor(vis, stepper);
  return integration_state(y, vis.state, save_state);
}

// 4. integrate_times: "Observer calls at given time points"
//
// NOTE: This could have been templated on the time iterator, but
// realistically we're only going to take
// std::vector<double>::iterator types anyway.
//
// NOTE: In this case, state is *always* saved.
//
// NOTE: In this case the output format is different with the main
// returned thing being the matrix of times, and the final state 'y'
// now an attribute.
template <typename Target>
class stepper_integrate_times : boost::static_visitor<> {
  typedef std::vector<double>::const_iterator Iterator;
public:
  typedef Target target_type;
  typedef typename Target::state_type state_type;
  Target target;
  state_type& y;
  Iterator times_start, times_end;
  double dt;
  state_saver<state_type> state;

  typedef void result_type;
  stepper_integrate_times(Target target_, state_type &y_,
                          Iterator times_start_, Iterator times_end_,
                          double dt_)
    : target(target_), y(y_),
      times_start(times_start_), times_end(times_end_), dt(dt_) {}
  void operator()(stepper_basic_euler s) {
    integrate_times(s);
  }
  void operator()(stepper_basic_modified_midpoint s) {
    integrate_times(s);
  }
  void operator()(stepper_basic_runge_kutta4 s) {
    integrate_times(s);
  }
  void operator()(stepper_basic_runge_kutta_cash_karp54 s) {
    integrate_times(s);
  }
  void operator()(stepper_basic_runge_kutta_fehlberg78 s) {
    integrate_times(s);
  }
  void operator()(stepper_basic_runge_kutta_dopri5 s) {
    integrate_times(s);
  }

  void operator()(stepper_controlled_runge_kutta_cash_karp54 s) {
    integrate_times(s);
  }
  void operator()(stepper_controlled_runge_kutta_fehlberg78 s) {
    integrate_times(s);
  }
  void operator()(stepper_controlled_runge_kutta_dopri5 s) {
    integrate_times(s);
  }
private:
  template <typename Stepper>
  void integrate_times(Stepper s) {
    using boost::numeric::odeint::integrate_times;
    state.steps =
      integrate_times(s, target, y, times_start, times_end, dt, state.obs);
  }
};

// NOTE: (again) that the return type here is different to all other
// integrate functions.  save_state is always true, we always want 'y'
// at the intermediate times, so the primary output is the matrix of
// 'y' with the final state saved as an attribute "y".
template <typename Target>
Rcpp::NumericMatrix
r_integrate_times(stepper stepper, Target target,
                  typename Target::state_type y,
                  std::vector<double> times, double dt) {
  check_dt(times.front(), times.back(), dt);
  if (!util::is_sorted(times.begin(), times.end(), dt > 0)) {
    if (times.front() != times.back()) { // corner case :-/
      std::string msg = "Times must be sorted ";
      Rcpp::stop(msg + (dt > 0 ? "(increasing)" : "(decreasing)"));
    }
  }

  stepper_integrate_times<Target>
    vis(target, y, times.begin(), times.end(), dt);
  boost::apply_visitor(vis, stepper);

  Rcpp::NumericMatrix ret = util::to_rcpp_matrix_by_row(vis.state.y);
  ret.attr("steps") = vis.state.steps;
  ret.attr("t")     = vis.state.t;
  ret.attr("y")     = Rcpp::NumericVector(y.begin(), y.end());
  return ret;
}

// 5. Convenience function
template <typename Target>
Rcpp::NumericVector
r_integrate_simple(Target target,
                   typename Target::state_type y,
                   double t0, double t1, double dt,
                   bool save_state=false) {
  using boost::numeric::odeint::integrate;
  check_dt(t0, t1, dt);

  state_saver<typename Target::state_type> state;
  if (save_state) {
    state.steps = integrate(target, y, t0, t1, dt, state.obs);
  } else {
    integrate(target, y, t0, t1, dt);
  }

  return integration_state(y, state, save_state);
}

}

#endif
