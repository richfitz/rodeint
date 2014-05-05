#ifndef _RODEINT_STEPPER_CONTROLLED_HPP_
#define _RODEINT_STEPPER_CONTROLLED_HPP_

// TODO: Going to need templating over state type -- for now
// everything is explicitly typedef-ed on std::vector<double>

// TODO: replace std::vector<double> with target_r::state_type, but
// wait until the more general target support is done.

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
typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_cash_karp54<
    std::vector<double> > >
stepper_controlled_runge_kutta_cash_karp54;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_fehlberg78<
    std::vector<double> > >
stepper_controlled_runge_kutta_fehlberg78;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_dopri5<
    std::vector<double> > >
stepper_controlled_runge_kutta_dopri5;

typedef
boost::variant<
  stepper_controlled_runge_kutta_cash_karp54,
  stepper_controlled_runge_kutta_fehlberg78,
  stepper_controlled_runge_kutta_dopri5>
stepper_controlled;

// This is more of a demonstration of how the approach will work more
// than anything else, really.  Gives a human readable version of the
// type of the controlled output stepper.
class stepper_controlled_type_visitor : boost::static_visitor<> {
public:
  typedef std::string result_type;
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

// For Rcpp
rodeint::stepper_controlled
stepper_controlled__ctor(std::string type,
                         double eps_abs, double eps_rel);


#endif
