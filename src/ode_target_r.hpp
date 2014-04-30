// #ifndef _RODEINT_ODE_TARGET_R_HPP_
// #define _RODEINT_ODE_TARGET_R_HPP_

// // First, organise the export of things from here.  I'm going to use
// // intrusive modification, I think.  The main trick is that I need to
// // use some Rcpp functions within the actual class, so this is more
// // complicated than it would seem necessary to be.

// #include <RcppCommon.h>
// #include <vector>

// namespace rodeint {
// class ode_target_r {
//   typedef std::vector<double> state_type;
//   typedef SEXP pars_type;
// public:
//   ode_target_r(SEXP derivs_, pars_type pars_);
//   void operator()(const state_type& y, state_type &dydt,
//                   const double t);
// private:
//   // This probably won't work without some magic:
//   Rcpp::Function derivs_;
//   SEXP pars;
// };
// }

// namespace Rcpp {
// template<>
// SEXP wrap(const rodeint::ode_target_r&);

// template<>
// rodeint::ode_target_r as(SEXP);
// }

// #include <Rcpp.h>


// #endif
