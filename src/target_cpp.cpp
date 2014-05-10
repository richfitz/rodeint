#include <rodeint/target_cpp.hpp>

// Note that this is actually the same as the target_r version, and we
// can get this done with a variant type I think.
// [[Rcpp::export]]
rodeint::target_cpp::state_type
target_cpp__derivs(Rcpp::XPtr<rodeint::target_cpp> target,
                   rodeint::target_cpp::state_type y,
                   double t) {
  rodeint::target_cpp::state_type dydt(y.size());
  (*target)(y, dydt, t);
  return dydt;
}

// [[Rcpp::export]]
rodeint::target_cpp::pars_type
target_cpp__get_pars(Rcpp::XPtr<rodeint::target_cpp> target) {
  return target->get_pars();
}

// [[Rcpp::export]]
void target_cpp__set_pars(Rcpp::XPtr<rodeint::target_cpp> target,
                          rodeint::target_cpp::pars_type pars) {
  target->set_pars(pars);
}
