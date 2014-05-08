#include "target_class.hpp"
#include <Rcpp.h>

// [[Rcpp::export]]
rodeint::target_class::pars_type
target_class__get_pars(Rcpp::XPtr<rodeint::target_class> target) {
  return target->get_pars();
}

// [[Rcpp::export]]
void target_class__set_pars(Rcpp::XPtr<rodeint::target_class> target,
                            rodeint::target_class::pars_type pars) {
  target->set_pars(pars);
}

// [[Rcpp::export]]
rodeint::target_class::state_type
target_class__derivs(Rcpp::XPtr<rodeint::target_class> target,
                     rodeint::target_class::state_type y,
                     double t) {
  rodeint::target_class::state_type dydt(y.size());
  (*target)(y, dydt, t);
  return dydt;
}
