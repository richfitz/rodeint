#include "target_r.hpp"
#include <boost/numeric/odeint.hpp>

// [[Rcpp::export]]
rodeint::target_r target_r__ctor(Rcpp::Function derivs,
                                 SEXP pars_type) {
  return rodeint::target_r(derivs, pars_type);
}

// [[Rcpp::export]]
rodeint::target_r::state_type 
target_r__derivs(Rcpp::XPtr<rodeint::target_r> target,
                 rodeint::target_r::state_type y, 
                 double t) {
  rodeint::target_r::state_type dydt(y.size());
  (*target)(y, dydt, t);
  return dydt;
}

// [[Rcpp::export]]
SEXP target_r__get_pars(Rcpp::XPtr<rodeint::target_r> target) {
  return target->get_pars();
}

// [[Rcpp::export]]
void target_r__set_pars(Rcpp::XPtr<rodeint::target_r> target,
                        SEXP pars) {
  target->set_pars(pars);
}
