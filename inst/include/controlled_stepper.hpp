#ifndef _RODEINT_CONTROLLED_STEPPER_HPP_
#define _RODEINT_CONTROLLED_STEPPER_HPP_

// TODO: Going to need templating over state type -- for now
// everything is explicitly typedef-ed on std::vector<double>

#include <boost/numeric/odeint.hpp>

// In theory, including just the required headers speeds things up --
// in practice this is about a 20% speedup, which seems a small reward
// for a lot of potential fragility.
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
controlled_stepper_runge_killta_cash_karp54;

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
  controlled_stepper_runge_killta_cash_karp54,
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
    return "runge_kutta_cash_karp45";
  }
  result_type operator()(const controlled_stepper_runge_kutta_fehlberg78&) const {
    return "runge_kutta_fehlberg78";
  }
  result_type operator()(const controlled_stepper_runge_kutta_dopri5&) const {
    return "runge_kutta_dopri5";
  }
};

}

#endif
