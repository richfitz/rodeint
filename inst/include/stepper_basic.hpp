#ifndef _RODEINT_STEPPER_BASIC_HPP_
#define _RODEINT_STEPPER_BASIC_HPP_

#include <boost/numeric/odeint.hpp>
#include <boost/variant.hpp>
#include <string>
#include <vector>

namespace rodeint {

typedef std::vector<double> stepper_basic_state_type;

typedef
boost::numeric::odeint::euler<stepper_basic_state_type>
stepper_basic_euler;

typedef
boost::numeric::odeint::modified_midpoint<stepper_basic_state_type>
stepper_basic_modified_midpoint;

typedef
boost::numeric::odeint::runge_kutta4<stepper_basic_state_type>
stepper_basic_runge_kutta4;

typedef
boost::numeric::odeint::runge_kutta_cash_karp54<stepper_basic_state_type>
stepper_basic_runge_kutta_cash_karp54;

typedef
boost::numeric::odeint::runge_kutta_fehlberg78<stepper_basic_state_type>
stepper_basic_runge_kutta_fehlberg78;

typedef
boost::numeric::odeint::runge_kutta_dopri5<stepper_basic_state_type>
stepper_basic_runge_kutta_dopri5;

typedef
boost::variant<
  stepper_basic_euler,
  stepper_basic_modified_midpoint,
  stepper_basic_runge_kutta4,
  stepper_basic_runge_kutta_cash_karp54,
  stepper_basic_runge_kutta_fehlberg78,
  stepper_basic_runge_kutta_dopri5>
stepper_basic;

// This is more of a demonstration of how the approach will work more
// than anything else, really.  Gives a human readable version of the
// type of the controlled output stepper.
class stepper_basic_type_visitor : boost::static_visitor<> {
public:
  typedef std::string result_type;
  result_type operator()(const stepper_basic_euler&) const {
    return "euler";
  }
  result_type operator()(const stepper_basic_modified_midpoint&) const {
    return "modified_midpoint";
  }
  result_type operator()(const stepper_basic_runge_kutta4&) const {
    return "runge_kutta4";
  }
  result_type operator()(const stepper_basic_runge_kutta_cash_karp54&) const {
    return "runge_kutta_cash_karp54";
  }
  result_type operator()(const stepper_basic_runge_kutta_fehlberg78&) const {
    return "runge_kutta_fehlberg78";
  }
  result_type operator()(const stepper_basic_runge_kutta_dopri5&) const {
    return "runge_kutta_dopri5";
  }
};

}

#endif
