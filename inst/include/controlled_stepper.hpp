#ifndef _RODEINT_CONTROLLED_STEPPER_HPP_
#define _RODEINT_CONTROLLED_STEPPER_HPP_

// TODO: Going to need templating over state type -- for now
// everything is explicitly typedef-ed on std::vector<double>

#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"
#include "observers.hpp"

// In theory, including just the required headers speeds things up --
// in practice this is about a 20% speedup, which seems a small reward
// for a lot of potential fragility.
//
//   #include <boost/numeric/odeint/stepper/controlled_runge_kutta.hpp>
//   #include <boost/numeric/odeint/stepper/runge_kutta_cash_karp54.hpp>
//   #include <boost/numeric/odeint/stepper/runge_kutta_fehlberg78.hpp>
//   #include <boost/numeric/odeint/stepper/runge_kutta_dopri5.hpp>
//   #include <boost/numeric/odeint/stepper/generation.hpp>

// Holding everything together with Boost.Variant - that way we can
// export a single type to R and still have compile time
// polymorphism/type checking on our side.  That's the idea anyway!
#include <boost/variant.hpp>
#include <string>
#include <vector>

namespace rodeint {
typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_cash_karp54<
    std::vector<double> > >
controlled_stepper_runge_kutta_cash_karp54;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_fehlberg78<
    std::vector<double> > >
controlled_stepper_runge_kutta_fehlberg78;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_dopri5<
    std::vector<double> > >
controlled_stepper_runge_kutta_dopri5;

typedef
boost::variant<
  controlled_stepper_runge_kutta_cash_karp54,
  controlled_stepper_runge_kutta_fehlberg78,
  controlled_stepper_runge_kutta_dopri5>
controlled_stepper;

// This is more of a demonstration of how the approach will work more
// than anything else, really.  Gives a human readable version of the
// type of the controlled output stepper.
class controlled_stepper_type_visitor : boost::static_visitor<> {
public:
  typedef std::string result_type;
  result_type operator()(const controlled_stepper_runge_kutta_cash_karp54&) const {
    return "runge_kutta_cash_karp54";
  }
  result_type operator()(const controlled_stepper_runge_kutta_fehlberg78&) const {
    return "runge_kutta_fehlberg78";
  }
  result_type operator()(const controlled_stepper_runge_kutta_dopri5&) const {
    return "runge_kutta_dopri5";
  }
};

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

}

#endif
