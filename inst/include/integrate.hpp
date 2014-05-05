#ifndef _RODEINT_INTEGRATE_HPP_
#define _RODEINT_INTEGRATE_HPP_

#include <boost/numeric/odeint.hpp>
#include <boost/variant.hpp>
#include <vector>
#include "target_r.hpp"
#include "observers.hpp"
#include "controlled_stepper.hpp"

namespace rodeint {

// There is quite a bit of repetition here, but it's not *that* bad.
// This gives us compile time polymorphism based on runtime input
// though, which is pretty sweet.
class controlled_stepper_integrate_adaptive : boost::static_visitor<> {
public:
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  double t0, t1, dt;
  bool save_state;
  // Always initialised, but only modified if save_state is true:
  size_t steps;
  std::vector<state_type> y_vec;
  std::vector<double> t_vec;
  obs_save_state<state_type> obs;

  typedef void result_type;
  controlled_stepper_integrate_adaptive(rodeint::target_r target_,
                                        state_type &y_,
                                        double t0_, double t1_,
                                        double dt_,
                                        bool save_state_)
    : target(target_), y(y_), t0(t0_), t1(t1_), dt(dt_),
      save_state(save_state_), steps(0), obs(y_vec, t_vec) {}
  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    integrate_adaptive(s);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    integrate_adaptive(s);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    integrate_adaptive(s);
  }
private:
  template <typename Stepper>
  void integrate_adaptive(Stepper s) {
    using boost::numeric::odeint::integrate_adaptive;
    if (save_state) {
      steps = integrate_adaptive(s, target, y, t0, t1, dt, obs);
    } else {
      integrate_adaptive(s, target, y, t0, t1, dt);
    }
  }
};

// TODO: More in keeping with the underlying odeint stuff if we
// template this on iterators.
//
// TODO: Could save some repetition if we store target/y/dt together
// and the observer stuff together, too.  That's showing repetition
// here and in integrate.cpp.  Get that fixed after the basic
// repetitive version works.
class controlled_stepper_integrate_times : boost::static_visitor<> {
public:
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  std::vector<double> times;
  double dt;
  // State here is always saved
  size_t steps;
  std::vector<state_type> y_vec;
  std::vector<double> t_vec;
  obs_save_state<state_type> obs;

  typedef void result_type;
  controlled_stepper_integrate_times(rodeint::target_r target_,
                                     state_type &y_,
                                     std::vector<double> times_,
                                     double dt_)
    : target(target_), y(y_), times(times_), dt(dt_),
      steps(0), obs(y_vec, t_vec) {}
  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    integrate_times(s);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    integrate_times(s);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    integrate_times(s);
  }
private:
  template <typename Stepper>
  void integrate_times(Stepper s) {
    using boost::numeric::odeint::integrate_times;
    steps = integrate_times(s, target, y, times.begin(), times.end(), dt, obs);
  }
};

}

#endif
