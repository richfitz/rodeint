#include "target_c.hpp"

void harmonic_oscillator_derivs(const std::vector<double>& y,
                                std::vector<double>& dydt,
                                const double /* t */,
                                const std::vector<double>& pars) {
  dydt[0] =  y[1];
  dydt[1] = -y[0] - pars[0] * y[1];
}

// [[Rcpp::export]]
rodeint::target_c target_c__ctor(rodeint::target_c::pars_type pars) {
  return rodeint::target_c(&harmonic_oscillator_derivs, pars);
}

// Note that this is actually the same as the target_r version, and we
// can get this done with a variant type I think.
// [[Rcpp::export]]
rodeint::target_c::state_type
target_c__derivs(Rcpp::XPtr<rodeint::target_c> target,
                 rodeint::target_c::state_type y,
                 double t) {
  rodeint::target_c::state_type dydt(y.size());
  (*target)(y, dydt, t);
  return dydt;
}

// [[Rcpp::export]]
rodeint::target_c::pars_type
target_c__get_pars(Rcpp::XPtr<rodeint::target_c> target) {
  return target->get_pars();
}

// [[Rcpp::export]]
void target_c__set_pars(Rcpp::XPtr<rodeint::target_c> target,
                        rodeint::target_c::pars_type pars) {
  target->set_pars(pars);
}
