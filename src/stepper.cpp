#include "stepper.hpp"
#include <Rcpp.h> // Rcpp::stop

// [[Rcpp::export]]
rodeint::stepper stepper__ctor(std::string type, std::string subtype,
                               double eps_abs, double eps_rel) {
  rodeint::stepper ret;
  if (type == "basic") {
    ret = stepper_basic__ctor(subtype);
  } else if (type == "controlled") {
    ret = stepper_controlled__ctor(subtype, eps_abs, eps_rel);
  } else {
    Rcpp::stop("Unknown type: " + type);
  }
  return ret;
}

// [[Rcpp::export]]
std::vector<std::string> stepper__type(rodeint::stepper s) {
  return boost::apply_visitor(rodeint::stepper_type_visitor(), s);
}
