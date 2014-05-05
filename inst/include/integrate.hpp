#ifndef _RODEINT_INTEGRATE_HPP_
#define _RODEINT_INTEGRATE_HPP_

#include <boost/numeric/odeint.hpp>
#include <boost/variant.hpp>
#include <vector>
#include "target_r.hpp"
#include "observers.hpp"
#include "controlled_stepper.hpp"

namespace rodeint {

// This is a helper for the little visitor classes below, and saves
// the state in a repeatable way.  It feels unduly complicated though,
// and there's probably a nicer way of doing it.
template <typename Visitor>
Rcpp::NumericVector integration_state(const Visitor* vis, bool save_state) {
  Rcpp::NumericVector ret(vis->y.begin(), vis->y.end());
  if (save_state) {
    vis->state.add_state(ret);
  }
  return ret;
}

// There is quite a bit of repetition here, but it's not *that* bad.
// This gives us compile time polymorphism based on runtime input
// though, which is pretty sweet.

// 1: integrate_const: "Equidistant observer calls"
class controlled_stepper_integrate_const : boost::static_visitor<> {
public:
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  double t0, t1, dt;
  bool save_state;
  state_saver<state_type> state;

  typedef void result_type;
  controlled_stepper_integrate_const(rodeint::target_r target_,
                                     state_type &y_,
                                     double t0_, double t1_,
                                     double dt_,
                                     bool save_state_)
    : target(target_), y(y_), t0(t0_), t1(t1_), dt(dt_),
      save_state(save_state_) {}
  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    integrate_const(s);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    integrate_const(s);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    integrate_const(s);
  }
  Rcpp::NumericVector r_state() const {
    return integration_state(this, save_state);
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

// 2. integrate_n_steps: "Integrate a given number of steps"
class controlled_stepper_integrate_n_steps : boost::static_visitor<> {
public:
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  double t0, dt;
  size_t n;
  bool save_state;
  state_saver<state_type> state;

  typedef void result_type;
  controlled_stepper_integrate_n_steps(rodeint::target_r target_,
                                       state_type &y_,
                                       double t0_, double dt_, size_t n_,
                                       bool save_state_)
    : target(target_), y(y_), t0(t0_), dt(dt_), n(n_),
      save_state(save_state_) {}
  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    integrate_n_steps(s);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    integrate_n_steps(s);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    integrate_n_steps(s);
  }
  Rcpp::NumericVector r_state() const {
    return integration_state(this, save_state);
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

// 3. integrate_adaptive "Observer calls at each step"
class controlled_stepper_integrate_adaptive : boost::static_visitor<> {
public:
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  double t0, t1, dt;
  bool save_state;
  state_saver<state_type> state;

  typedef void result_type;
  controlled_stepper_integrate_adaptive(rodeint::target_r target_,
                                        state_type &y_,
                                        double t0_, double t1_,
                                        double dt_,
                                        bool save_state_)
    : target(target_), y(y_), t0(t0_), t1(t1_), dt(dt_),
      save_state(save_state_) {}
  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    integrate_adaptive(s);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    integrate_adaptive(s);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    integrate_adaptive(s);
  }
  Rcpp::NumericVector r_state() const {
    return integration_state(this, save_state);
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

// 4. integrate_times: "Observer calls at given time points"
//
// NOTE: This could have been templated on the time iterator, but
// realistically we're only going to take
// std::vector<double>::iterator types anyway.
//
// NOTE: In this case, state is *always* saved.
class controlled_stepper_integrate_times : boost::static_visitor<> {
  typedef std::vector<double>::const_iterator Iterator;
public:
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  Iterator times_start, times_end;
  double dt;
  bool save_state; // NOTE: always true
  state_saver<state_type> state;

  typedef void result_type;
  controlled_stepper_integrate_times(rodeint::target_r target_,
                                     state_type &y_,
                                     Iterator times_start_,
                                     Iterator times_end_,
                                     double dt_)
    : target(target_), y(y_),
      times_start(times_start_), times_end(times_end_), dt(dt_),
      save_state(true) {}
  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    integrate_times(s);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    integrate_times(s);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    integrate_times(s);
  }
  Rcpp::NumericVector r_state() const {
    return integration_state(this, save_state);
  }
private:
  template <typename Stepper>
  void integrate_times(Stepper s) {
    using boost::numeric::odeint::integrate_times;
    state.steps =
      integrate_times(s, target, y, times_start, times_end, dt, state.obs);
  }
};

}

#endif
