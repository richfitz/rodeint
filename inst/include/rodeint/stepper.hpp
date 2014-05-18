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
#include <boost/numeric/odeint/stepper/bulirsch_stoer.hpp>
#include <boost/numeric/odeint/stepper/bulirsch_stoer_dense_out.hpp>
#include <boost/numeric/odeint/stepper/controlled_runge_kutta.hpp>
#include <boost/numeric/odeint/stepper/dense_output_runge_kutta.hpp>
#include <boost/numeric/odeint/stepper/generation.hpp>
#endif

#include <string>
#include <vector>

#include <rodeint/common.hpp>
#include <boost/any.hpp>

namespace rodeint {

// 1. Basic stepper algorithms
typedef
boost::numeric::odeint::euler<vector_stl>
stepper_basic_euler_stl;

typedef
boost::numeric::odeint::modified_midpoint<vector_stl>
stepper_basic_modified_midpoint_stl;

typedef
boost::numeric::odeint::runge_kutta4<vector_stl>
stepper_basic_runge_kutta4_stl;

typedef
boost::numeric::odeint::runge_kutta_cash_karp54<vector_stl>
stepper_basic_runge_kutta_cash_karp54_stl;

typedef
boost::numeric::odeint::runge_kutta_fehlberg78<vector_stl>
stepper_basic_runge_kutta_fehlberg78_stl;

typedef
boost::numeric::odeint::runge_kutta_dopri5<vector_stl>
stepper_basic_runge_kutta_dopri5_stl;

// 2. Controlled stepper algorithms
typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_cash_karp54<vector_stl> >
stepper_controlled_runge_kutta_cash_karp54_stl;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_fehlberg78<vector_stl> >
stepper_controlled_runge_kutta_fehlberg78_stl;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_dopri5<vector_stl> >
stepper_controlled_runge_kutta_dopri5_stl;

typedef
boost::numeric::odeint::bulirsch_stoer<vector_stl>
stepper_controlled_bulirsch_stoer_stl;

// 3. Dense stepper algorithms
typedef stepper_basic_euler_stl stepper_dense_euler_stl;

typedef
boost::numeric::odeint::dense_output_runge_kutta<
  stepper_controlled_runge_kutta_dopri5_stl>
stepper_dense_runge_kutta_dopri5_stl;

typedef
boost::numeric::odeint::bulirsch_stoer_dense_out<vector_stl>
stepper_dense_bulirsch_stoer_stl;

// uBLAS things
// 1. Basic steppers
typedef
boost::numeric::odeint::euler<vector_ublas>
stepper_basic_euler_ublas;

typedef
boost::numeric::odeint::modified_midpoint<vector_ublas>
stepper_basic_modified_midpoint_ublas;

typedef
boost::numeric::odeint::runge_kutta4<vector_ublas>
stepper_basic_runge_kutta4_ublas;

typedef
boost::numeric::odeint::runge_kutta_cash_karp54<vector_ublas>
stepper_basic_runge_kutta_cash_karp54_ublas;

typedef
boost::numeric::odeint::runge_kutta_fehlberg78<vector_ublas>
stepper_basic_runge_kutta_fehlberg78_ublas;

typedef
boost::numeric::odeint::runge_kutta_dopri5<vector_ublas>
stepper_basic_runge_kutta_dopri5_ublas;

// NOTE: Not available for stl steppers
typedef
boost::numeric::odeint::rosenbrock4<double>
stepper_basic_rosenbrock4_ublas;

// 2. Controlled steppers
typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_cash_karp54<vector_ublas> >
stepper_controlled_runge_kutta_cash_karp54_ublas;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_fehlberg78<vector_ublas> >
stepper_controlled_runge_kutta_fehlberg78_ublas;

typedef
boost::numeric::odeint::controlled_runge_kutta<
  boost::numeric::odeint::runge_kutta_dopri5<vector_ublas> >
stepper_controlled_runge_kutta_dopri5_ublas;

typedef
boost::numeric::odeint::bulirsch_stoer<vector_ublas>
stepper_controlled_bulirsch_stoer_ublas;

// NOTE: Not available for stl steppers
typedef
boost::numeric::odeint::rosenbrock4_controller<
  stepper_basic_rosenbrock4_ublas>
stepper_controlled_rosenbrock4_ublas;

// 3. Dense steppers
typedef stepper_basic_euler_ublas stepper_dense_euler_ublas;

typedef
boost::numeric::odeint::dense_output_runge_kutta<
  stepper_controlled_runge_kutta_dopri5_ublas>
stepper_dense_runge_kutta_dopri5_ublas;

typedef
boost::numeric::odeint::bulirsch_stoer_dense_out<vector_ublas>
stepper_dense_bulirsch_stoer_ublas;

typedef
boost::numeric::odeint::rosenbrock4_dense_output<
  stepper_controlled_rosenbrock4_ublas>
stepper_dense_rosenbrock4_ublas;

class stepper {
public:
  // TODO: No need for these to be in u.c.
  // TODO: Can generate these lists and the ones in the cpp file via
  // cog, which would ensure the order is *always* correct.
  enum Category {BASIC, CONTROLLED, DENSE};
  enum Algorithm {EULER,
                  MODIFIED_MIDPOINT,
                  RUNGE_KUTTA4,
                  RUNGE_KUTTA_CASH_KARP54,
                  RUNGE_KUTTA_FEHLBERG78,
                  RUNGE_KUTTA_DOPRI5,
                  BULIRSCH_STOER,
                  ROSENBROCK4};

  // Construction from human-readable things (used from R)
  stepper(std::string category_, std::string algorithm_, bool ublas_state_,
          double abs_tol_, double rel_tol_)
    : category(category_from_string(category_)),
      algorithm(algorithm_from_string(algorithm_)),
      ublas_state(ublas_state_),
      abs_tol(abs_tol_), rel_tol(rel_tol_),
      stepper_odeint(construct(category, algorithm, ublas_state,
                               abs_tol, rel_tol)) { }
  // Might make this private (also nice if we had delegated
  // constructors)
  stepper(Category category_, Algorithm algorithm_, bool ublas_state_,
          double abs_tol_, double rel_tol_)
    : category(category_), algorithm(algorithm_), ublas_state(ublas_state_),
      abs_tol(abs_tol_), rel_tol(rel_tol_),
      stepper_odeint(construct(category, algorithm, ublas_state,
                               abs_tol, rel_tol)) { }
  std::string category_name() const {
    return category_name(category);
  }
  std::string algorithm_name() const {
    return algorithm_name(algorithm);
  }
  // Used in tests
  Category category_id() const {
    return category;
  }
  Algorithm algorithm_id() const {
    return algorithm;
  }
  bool has_ublas_state() const {
    return ublas_state;
  }
  bool algorithm_needs_jacobian() const {
    return needs_jacobian[algorithm];
  }

  // This is the little accessor:
  template <typename Stepper>
  Stepper as() const {
    try {
      Stepper ret = boost::any_cast<Stepper>(stepper_odeint);
      return ret;
    } catch (const boost::bad_any_cast&) {
      util::stop("Failed to get stepper");
      return Stepper(); // Won't get here.
    }
  }
  template <typename Stepper>
  Stepper as(bool ublas_state_) {
    // Enforce the right state type
    if (ublas_state != ublas_state_) {
      stepper_odeint =
        construct(category, algorithm, ublas_state_, abs_tol, rel_tol);
    }
    return as<Stepper>();
  }
private:
  // Actual stepper information.
  Category   category;
  Algorithm  algorithm;
  bool       ublas_state;
  double     abs_tol;
  double     rel_tol;
  boost::any stepper_odeint;

  // Information about the different algorithms.  This idea might change.
  const static bool ok_basic[];
  const static bool ok_controlled[];
  const static bool ok_dense[];
  const static bool needs_jacobian[];

  // Check a bunch of stuff on intitialisation
  static std::string category_name(Category category);
  static std::string algorithm_name(Algorithm algorithm);
  static void validate(Category category, Algorithm algorithm,
                       bool ublas_state,
                       double abs_tol, double rel_tol);
  static boost::any construct(Category category, Algorithm algorithm,
                              bool ublas_state,
                              double abs_tol, double rel_tol);
  template <typename T>
  static boost::any construct(Category category, Algorithm algorithm,
                              double abs_tol, double rel_tol);
  template <typename T>
  static boost::any construct_basic(Algorithm algorithm);
  template <typename T>
  static boost::any construct_controlled(Algorithm algorithm,
                                         double abs_tol, double rel_tol);
  template <typename T>
  static boost::any construct_dense(Algorithm algorithm,
                                    double abs_tol, double rel_tol);

  // Used during initialisation from R to translate names into
  // strings.  The tests will check that we do actually agree.
  static Category category_from_string(const std::string& x);
  static Algorithm algorithm_from_string(const std::string& x);
};

template <typename T>
boost::any stepper::construct(stepper::Category category,
                              stepper::Algorithm algorithm,
                              double abs_tol, double rel_tol) {
  switch(category) {
  case BASIC:
    return construct_basic<T>(algorithm);
  case CONTROLLED:
    return construct_controlled<T>(algorithm, abs_tol, rel_tol);
  case DENSE:
    return construct_dense<T>(algorithm, abs_tol, rel_tol);
  default:
    util::stop("Invalid category"); // defensive
  }
  return boost::any(); // Won't get here.
}

}

#endif
