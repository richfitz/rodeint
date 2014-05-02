#ifndef _RODEINT_CONTROLLED_STEPPER_HPP_
#define _RODEINT_CONTROLLED_STEPPER_HPP_

// TODO: Going to need templating over state type -- for now
// everything is explicitly typedef-ed on std::vector<double>

#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"

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
class controlled_stepper_integrate_adaptive_visitor :
    boost::static_visitor<> {
  typedef rodeint::target_r::state_type state_type;
  rodeint::target_r target;
  state_type& y;
  double t0, t1, dt;
  size_t steps;
public:
  typedef void result_type;
  controlled_stepper_integrate_adaptive_visitor(rodeint::target_r target_,
                                                state_type &y_,
                                                double t0_, double t1_,
                                                double dt_)
    : target(target_), y(y_), t0(t0_), t1(t1_), dt(dt_), steps(0) {}

  void operator()(controlled_stepper_runge_kutta_cash_karp54 s) {
    steps =
      boost::numeric::odeint::integrate_adaptive(s, target, y, t0, t1, dt);
  }
  void operator()(controlled_stepper_runge_kutta_fehlberg78 s) {
    steps =
      boost::numeric::odeint::integrate_adaptive(s, target, y, t0, t1, dt);
  }
  void operator()(controlled_stepper_runge_kutta_dopri5 s) {
    steps =
      boost::numeric::odeint::integrate_adaptive(s, target, y, t0, t1, dt);
  }
  size_t last_steps() const { return steps; }
};

// Template:
// class controlled_stepper_XXX_visitor : boost::static_visitor<> {
// public:
//   result_type operator()(const controlled_stepper_runge_kutta_cash_karp54&) const {
//   }
//   result_type operator()(const controlled_stepper_runge_kutta_fehlberg78&) const {
//   }
//   result_type operator()(const controlled_stepper_runge_kutta_dopri5&) const {
//   }
// };

}

#endif
