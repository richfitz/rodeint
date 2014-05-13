#ifndef _RODEINT_STEPPER_HPP_
#define _RODEINT_STEPPER_HPP_

// Following advice here
//   http://www.boost.org/doc/libs/1_55_0/libs/numeric/odeint/doc/html/boost_numeric_odeint/getting_started/usage__compilation__headers.html
// though I suspect we use so much of odeint that it's a lost cause.
#ifdef ODEINT_INCLUDE_EVERYTHING
#include <boost/numeric/odeint.hpp>
#else
#include <boost/numeric/odeint/stepper/euler.hpp>
#include <boost/numeric/odeint/stepper/modified_midpoint.hpp>
#include <boost/numeric/odeint/stepper/runge_kutta4.hpp>
#include <boost/numeric/odeint/stepper/runge_kutta_cash_karp54.hpp>
#include <boost/numeric/odeint/stepper/runge_kutta_fehlberg78.hpp>
#include <boost/numeric/odeint/stepper/runge_kutta_dopri5.hpp>
#include <boost/numeric/odeint/stepper/controlled_runge_kutta.hpp>
#include <boost/numeric/odeint/stepper/generation.hpp>
#endif

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

// 2. Controlled stepper types
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
  stepper_basic_euler,
  stepper_basic_modified_midpoint,
  stepper_basic_runge_kutta4,
  stepper_basic_runge_kutta_cash_karp54,
  stepper_basic_runge_kutta_fehlberg78,
  stepper_basic_runge_kutta_dopri5,
  stepper_controlled_runge_kutta_cash_karp54,
  stepper_controlled_runge_kutta_fehlberg78,
  stepper_controlled_runge_kutta_dopri5>
stepper;

// Stiff stepper (only rosenbrock supported, requiring ublas vectors)
// typedef boost::numeric::ublas::vector<double> stepper_stiff_state_type;
typedef double stepper_stiff_state_type;

typedef
boost::numeric::odeint::rosenbrock4<double>
stepper_basic_rosenbrock4;

typedef
boost::numeric::odeint::rosenbrock4_controller<
  stepper_basic_rosenbrock4>
stepper_controlled_rosenbrock4;

typedef
boost::numeric::odeint::rosenbrock4_dense_output<
  stepper_controlled_rosenbrock4>
stepper_dense_rosenbrock4;

typedef boost::variant<
  stepper_basic_rosenbrock4,
  stepper_controlled_rosenbrock4,
  stepper_dense_rosenbrock4>
stepper_stiff;

// This is more of a demonstration of how the approach will work more
// than anything else, really.  Gives a human readable version of the
// type of the controlled output stepper.
//
// An alternative way of doing this would be to have two versions --
// one for the category, one for the type.  But the information is all
// stored in the reference class object anyway.
class stepper_type_visitor : boost::static_visitor<> {
public:
  typedef std::vector<std::string> result_type;
  result_type operator()(const stepper_basic_euler&) const {
    return join_types("basic", "euler");
  }
  result_type operator()(const stepper_basic_modified_midpoint&) const {
    return join_types("basic", "modified_midpoint");
  }
  result_type operator()(const stepper_basic_runge_kutta4&) const {
    return join_types("basic", "runge_kutta4");
  }
  result_type operator()(const stepper_basic_runge_kutta_cash_karp54&) const {
    return join_types("basic", "runge_kutta_cash_karp54");
  }
  result_type operator()(const stepper_basic_runge_kutta_fehlberg78&) const {
    return join_types("basic", "runge_kutta_fehlberg78");
  }
  result_type operator()(const stepper_basic_runge_kutta_dopri5&) const {
    return join_types("basic", "runge_kutta_dopri5");
  }

  result_type operator()(const stepper_controlled_runge_kutta_cash_karp54&) const {
    return join_types("controlled", "runge_kutta_cash_karp54");
  }
  result_type operator()(const stepper_controlled_runge_kutta_fehlberg78&) const {
    return join_types("controlled", "runge_kutta_fehlberg78");
  }
  result_type operator()(const stepper_controlled_runge_kutta_dopri5&) const {
    return join_types("controlled", "runge_kutta_dopri5");
  }

private:
  static std::vector<std::string> join_types(const std::string& category,
                                             const std::string& type) {
    std::vector<std::string> ret;
    ret.push_back(category);
    ret.push_back(type);
    return ret;
  }
};

}

#endif
