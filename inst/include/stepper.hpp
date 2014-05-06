#ifndef _RODEINT_STEPPER_HPP_
#define _RODEINT_STEPPER_HPP_

#include <boost/numeric/odeint.hpp>

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
typedef std::vector<double> stepper_state_type;

// 1. Basic stepper types.
typedef
boost::numeric::odeint::modified_midpoint<stepper_state_type>
stepper_basic_modified_midpoint;

typedef
boost::numeric::odeint::runge_kutta4<stepper_state_type>
stepper_basic_runge_kutta4;

typedef
boost::numeric::odeint::runge_kutta_cash_karp54<stepper_state_type>
stepper_basic_runge_kutta_cash_karp54;

typedef
boost::numeric::odeint::runge_kutta_fehlberg78<stepper_state_type>
stepper_basic_runge_kutta_fehlberg78;

typedef
boost::numeric::odeint::runge_kutta_dopri5<stepper_state_type>
stepper_basic_runge_kutta_dopri5;

// 2. Controlld stepper types
typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_cash_karp54<stepper_state_type> >
stepper_controlled_runge_kutta_cash_karp54;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_fehlberg78<stepper_state_type> >
stepper_controlled_runge_kutta_fehlberg78;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_dopri5<stepper_state_type> >
stepper_controlled_runge_kutta_dopri5;

typedef
boost::numeric::odeint::euler<stepper_state_type>
stepper_basic_euler;

typedef
boost::variant<
  stepper_basic_runge_kutta4,
  stepper_controlled_runge_kutta_cash_karp54,
  stepper_controlled_runge_kutta_fehlberg78,
  stepper_controlled_runge_kutta_dopri5>
stepper;

// This is more of a demonstration of how the approach will work more
// than anything else, really.  Gives a human readable version of the
// type of the controlled output stepper.
class stepper_type_visitor : boost::static_visitor<> {
public:
  typedef std::string result_type;
  result_type operator()(const stepper_basic_runge_kutta4&) const {
    return "runge_kutta4";
  }

  result_type operator()(const stepper_controlled_runge_kutta_cash_karp54&) const {
    return "runge_kutta_cash_karp54";
  }
  result_type operator()(const stepper_controlled_runge_kutta_fehlberg78&) const {
    return "runge_kutta_fehlberg78";
  }
  result_type operator()(const stepper_controlled_runge_kutta_dopri5&) const {
    return "runge_kutta_dopri5";
  }
};

}

#endif
