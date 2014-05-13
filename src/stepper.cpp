#include <rodeint/stepper.hpp>
#include <Rcpp.h> // Rcpp::stop

// [[Rcpp::export]]
rodeint::stepper
stepper_controlled__ctor(std::string type,
                         double eps_abs, double eps_rel) {
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::runge_kutta_cash_karp54;
  using boost::numeric::odeint::runge_kutta_fehlberg78;
  using boost::numeric::odeint::runge_kutta_dopri5;

  rodeint::stepper ret;
  typedef rodeint::stepper_state_type state;

  if (type == "runge_kutta_cash_karp54") {
    ret = make_controlled<runge_kutta_cash_karp54<state> >(eps_abs, eps_rel);
  } else if (type == "runge_kutta_fehlberg78") {
    ret = make_controlled<runge_kutta_fehlberg78<state> >(eps_abs, eps_rel);
  } else if (type == "runge_kutta_dopri5") {
    ret = make_controlled<runge_kutta_dopri5<state> >(eps_abs, eps_rel);
  } else {
    Rcpp::stop("Unknown type: " + type);
  }

  return ret;
}

// [[Rcpp::export]]
rodeint::stepper
stepper_basic__ctor(std::string type) {
  rodeint::stepper ret;
  typedef rodeint::stepper_state_type state;

  if (type == "euler") {
    ret = boost::numeric::odeint::euler<state>();
  } else if (type == "modified_midpoint") {
    ret = boost::numeric::odeint::modified_midpoint<state>();
  } else if (type == "runge_kutta4") {
    ret = boost::numeric::odeint::runge_kutta4<state>();
  } else if (type == "runge_kutta_cash_karp54") {
    ret = boost::numeric::odeint::runge_kutta_cash_karp54<state>();
  } else if (type == "runge_kutta_fehlberg78") {
    ret = boost::numeric::odeint::runge_kutta_fehlberg78<state>();
  } else if (type == "runge_kutta_dopri5") {
    ret = boost::numeric::odeint::runge_kutta_dopri5<state>();
  } else {
    Rcpp::stop("Unknown type: " + type);
  }
  return ret;
}

// [[Rcpp::export]]
rodeint::stepper_stiff
stepper_stiff__ctor(std::string category,
                    double eps_abs, double eps_rel) {
  using boost::numeric::odeint::rosenbrock4;
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::make_dense_output;

  typedef double state;

  // I'm unclear why the non-multiple exit approach does not work.
  if (category == "basic") {
    rodeint::stepper_basic_rosenbrock4 stepper =
      rosenbrock4<double>();
    return rodeint::stepper_stiff(stepper);
  } else if (category == "controlled") {
    rodeint::stepper_controlled_rosenbrock4 stepper =
      make_controlled<rosenbrock4<double > >(eps_abs, eps_rel);
    return rodeint::stepper_stiff(stepper);
  } else if (category == "dense") {
    rodeint::stepper_dense_rosenbrock4 stepper =
      make_dense_output<rosenbrock4<double > >(eps_abs, eps_rel);
    return rodeint::stepper_stiff(stepper);
  }

  Rcpp::stop("Unknown category: " + category);
  return rodeint::stepper_stiff();
}

// This is going to change considerably, and might go into the
// general controlled system...
//
// TODO: check and change eps_abs / eps_rel -> atol, rtol everywhere.
// rodeint::stepper_stiff
// stepper_stiff__ctor(std::string /* type */, // currently ignored
//                     double eps_abs, double eps_rel) {
//   return ///rodeint::stepper_stiff_controlled(eps_abs, eps_rel);

//   rodeint::stepper_stiff ret(make_dense_output<rosenbrock4<double >>
//                              (eps_abs, eps_rel));
//   return ret;
// }

// [[Rcpp::export]]
std::vector<std::string> stepper__type(rodeint::stepper s) {
  return boost::apply_visitor(rodeint::stepper_type_visitor(), s);
}
