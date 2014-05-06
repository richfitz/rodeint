#include "stepper.hpp"
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

  if (type == "runge_kutta4") {
    ret = boost::numeric::odeint::runge_kutta4<state>();
  } else {
    Rcpp::stop("Unknown type: " + type);
  }
  return ret;
}

// [[Rcpp::export]]
std::string stepper__type(rodeint::stepper s) {
  return boost::apply_visitor(rodeint::stepper_type_visitor(), s);
}
