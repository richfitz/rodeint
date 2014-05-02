#include "controlled_stepper.hpp"
#include <Rcpp.h> // Rcpp::stop

// [[Rcpp::export]]
rodeint::controlled_stepper
controlled_stepper__ctor(std::string type,
                         double eps_abs, double eps_rel) {
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::runge_kutta_cash_karp54;
  using boost::numeric::odeint::runge_kutta_fehlberg78;
  using boost::numeric::odeint::runge_kutta_dopri5;

  typedef std::vector<double> state;

  rodeint::controlled_stepper ret;

  if (type == "runge_kutta_cash_karp54") {
    ret = make_controlled<runge_kutta_cash_karp54<state> >(eps_abs, eps_rel);
  } else if (type == "runge_kutta_fehlberg78") {
    ret = make_controlled<runge_kutta_fehlberg78<state> >(eps_abs, eps_rel);
  } else if (type == "runge_kutta_dopri5") {
    ret = make_controlled<runge_kutta_dopri5<state> >(eps_abs, eps_rel);
  } else {
    Rcpp::stop("Unknown type");
  }

  return ret;
}

// [[Rcpp::export]]
std::string controlled_stepper__type(rodeint::controlled_stepper s) {
  return boost::apply_visitor(rodeint::controlled_stepper_type_visitor(), s);
}

