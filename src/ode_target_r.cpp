// #include "ode_target_r.hpp"
// #include <Rcpp.h>

// namespace rodeint {

// ode_target_r::ode_target_r(SEXP derivs_, pars_type pars_)
//   : derivs(Rcpp::as<Rcpp::Function>(derivs_)), pars(pars_) {
// }

// void ode_target_r::operator()(const state_type& y, state_type &dydt,
//                               const double t) {
//   // This obviously does more copying than seems necessary, but even
//   // more may happen on the R side...
//   const state_type ret = derivs(Rcpp::wrap(t), Rcpp::wrap(y), pars);
//   std::copy(ret.begin(), ret.end(), dydt.end());
// }

// }

// namespace Rcpp {
// template<>
// SEXP wrap(const rodeint::ode_target_r& obj) {
//   return Rcpp::wrap(Rcpp::Xptr<rodeint::ode_target_r>(obj));
// }

// template<>
// rodeint::ode_target_r as(SEXP) {
  
// }
// }

// }
