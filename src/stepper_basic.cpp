#include "stepper_basic.hpp"
#include <Rcpp.h> // Rcpp::stop

// [[Rcpp::export]]
rodeint::stepper_basic
stepper_basic__ctor(std::string type) {
  rodeint::stepper_basic ret;
  if (type == "euler") {
    ret = rodeint::stepper_basic_euler();
  } else if (type == "modified_midpoint") {
    ret = rodeint::stepper_basic_modified_midpoint();
  } else if (type == "runge_kutta4") {
    ret = rodeint::stepper_basic_runge_kutta4();
  } else if (type == "runge_kutta_cash_karp54") {
    ret = rodeint::stepper_basic_runge_kutta_cash_karp54();
  } else if (type == "runge_kutta_fehlberg78") {
    ret = rodeint::stepper_basic_runge_kutta_fehlberg78();
  } else if (type == "runge_kutta_dopri5") {
    ret = rodeint::stepper_basic_runge_kutta_dopri5();
  } else {
    Rcpp::stop("Unknown type " + type);
  }
  return ret;
}

// [[Rcpp::export]]
std::string stepper_basic__type(rodeint::stepper_basic s) {
  return boost::apply_visitor(rodeint::stepper_basic_type_visitor(), s);
}
