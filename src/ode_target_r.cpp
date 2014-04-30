#include <boost/numeric/odeint.hpp>
#include "ode_target_r.hpp"

namespace rodeint {

ode_target_r::ode_target_r(Rcpp::Function derivs_, SEXP pars_)
  : derivs(derivs_), pars(pars_) {
}

void ode_target_r::operator()(const state_type& y, state_type &dydt,
                              const double t) {
  const state_type ret = 
    Rcpp::as<state_type>(derivs(Rcpp::wrap(t), Rcpp::wrap(y), pars));
  std::copy(ret.begin(), ret.end(), dydt.begin());
}

void ode_target_r::set_pars(SEXP pars_) {
  pars = pars_;
}

}

// Then the exports
// [[Rcpp::export]]
rodeint::ode_target_r ode_target_r_make(Rcpp::Function derivs,
                                        SEXP pars_type) {
  return rodeint::ode_target_r(derivs, pars_type);
}

// TODO: use 'rodeint::ode_target_r::state_type' for argument and return.
// [[Rcpp::export]]
std::vector<double> ode_target_r_derivs(Rcpp::XPtr<rodeint::ode_target_r> obj,
                                        std::vector<double> y, 
                                        double t) {
  std::vector<double> dydt(y.size());
  (*obj)(y, dydt, t);
  return dydt;
}

// [[Rcpp::export]]
void ode_target_r_set_pars(Rcpp::XPtr<rodeint::ode_target_r> obj,
                           SEXP pars) {
  obj->set_pars(pars);
}

// [[Rcpp::export]]
std::vector<double> ode_target_r_basic(rodeint::ode_target_r obj,
                                       std::vector<double> y, 
                                       double t0, double t1,
                                       double dt) {
  boost::numeric::odeint::integrate(obj, y, t0, t1, dt);
  return y;
}
