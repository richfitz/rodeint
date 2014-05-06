// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rodeint.h"
#include <Rcpp.h>

using namespace Rcpp;

// r_integrate_const_r
Rcpp::NumericVector r_integrate_const_r(rodeint::stepper_controlled stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_r_integrate_const_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_const_r(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_const_cpp
Rcpp::NumericVector r_integrate_const_cpp(rodeint::stepper_controlled stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_r_integrate_const_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_const_cpp(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_n_steps_r
Rcpp::NumericVector r_integrate_n_steps_r(rodeint::stepper_controlled stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double dt, size_t n, bool save_state);
RcppExport SEXP rodeint_r_integrate_n_steps_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP dtSEXP, SEXP nSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< size_t >::type n(nSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_n_steps_r(stepper, target, y, t0, dt, n, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_n_steps_cpp
Rcpp::NumericVector r_integrate_n_steps_cpp(rodeint::stepper_controlled stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double dt, size_t n, bool save_state);
RcppExport SEXP rodeint_r_integrate_n_steps_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP dtSEXP, SEXP nSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< size_t >::type n(nSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_n_steps_cpp(stepper, target, y, t0, dt, n, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_adaptive_r
Rcpp::NumericVector r_integrate_adaptive_r(rodeint::stepper_controlled stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_r_integrate_adaptive_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_adaptive_r(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_adaptive_cpp
Rcpp::NumericVector r_integrate_adaptive_cpp(rodeint::stepper_controlled stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_r_integrate_adaptive_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_adaptive_cpp(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_times_r
Rcpp::NumericVector r_integrate_times_r(rodeint::stepper_controlled stepper, rodeint::target_r target, rodeint::target_r::state_type y, std::vector<double> times, double dt);
RcppExport SEXP rodeint_r_integrate_times_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP timesSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type times(timesSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::NumericVector __result = r_integrate_times_r(stepper, target, y, times, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_times_cpp
Rcpp::NumericVector r_integrate_times_cpp(rodeint::stepper_controlled stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, std::vector<double> times, double dt);
RcppExport SEXP rodeint_r_integrate_times_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP timesSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type times(timesSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::NumericVector __result = r_integrate_times_cpp(stepper, target, y, times, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_simple_r
Rcpp::NumericVector r_integrate_simple_r(rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_r_integrate_simple_r(SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_simple_r(target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_simple_cpp
Rcpp::NumericVector r_integrate_simple_cpp(rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_r_integrate_simple_cpp(SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_simple_cpp(target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stepper_controlled__ctor
rodeint::stepper_controlled stepper_controlled__ctor(std::string type, double eps_abs, double eps_rel);
RcppExport SEXP rodeint_stepper_controlled__ctor(SEXP typeSEXP, SEXP eps_absSEXP, SEXP eps_relSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type type(typeSEXP );
        Rcpp::traits::input_parameter< double >::type eps_abs(eps_absSEXP );
        Rcpp::traits::input_parameter< double >::type eps_rel(eps_relSEXP );
        rodeint::stepper_controlled __result = stepper_controlled__ctor(type, eps_abs, eps_rel);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stepper_controlled__type
std::string stepper_controlled__type(rodeint::stepper_controlled s);
RcppExport SEXP rodeint_stepper_controlled__type(SEXP sSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper_controlled >::type s(sSEXP );
        std::string __result = stepper_controlled__type(s);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_cpp__derivs
rodeint::target_cpp::state_type target_cpp__derivs(Rcpp::XPtr<rodeint::target_cpp> target, rodeint::target_cpp::state_type y, double t);
RcppExport SEXP rodeint_target_cpp__derivs(SEXP targetSEXP, SEXP ySEXP, SEXP tSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_cpp> >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t(tSEXP );
        rodeint::target_cpp::state_type __result = target_cpp__derivs(target, y, t);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_cpp__get_pars
rodeint::target_cpp::pars_type target_cpp__get_pars(Rcpp::XPtr<rodeint::target_cpp> target);
RcppExport SEXP rodeint_target_cpp__get_pars(SEXP targetSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_cpp> >::type target(targetSEXP );
        rodeint::target_cpp::pars_type __result = target_cpp__get_pars(target);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_cpp__set_pars
void target_cpp__set_pars(Rcpp::XPtr<rodeint::target_cpp> target, rodeint::target_cpp::pars_type pars);
RcppExport SEXP rodeint_target_cpp__set_pars(SEXP targetSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_cpp> >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::pars_type >::type pars(parsSEXP );
        target_cpp__set_pars(target, pars);
    }
    return R_NilValue;
END_RCPP
}
// target_r__ctor
rodeint::target_r target_r__ctor(Rcpp::Function derivs, SEXP pars_type);
RcppExport SEXP rodeint_target_r__ctor(SEXP derivsSEXP, SEXP pars_typeSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::Function >::type derivs(derivsSEXP );
        Rcpp::traits::input_parameter< SEXP >::type pars_type(pars_typeSEXP );
        rodeint::target_r __result = target_r__ctor(derivs, pars_type);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_r__derivs
rodeint::target_r::state_type target_r__derivs(Rcpp::XPtr<rodeint::target_r> target, rodeint::target_r::state_type y, double t);
RcppExport SEXP rodeint_target_r__derivs(SEXP targetSEXP, SEXP ySEXP, SEXP tSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_r> >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t(tSEXP );
        rodeint::target_r::state_type __result = target_r__derivs(target, y, t);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_r__get_pars
SEXP target_r__get_pars(Rcpp::XPtr<rodeint::target_r> target);
RcppExport SEXP rodeint_target_r__get_pars(SEXP targetSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_r> >::type target(targetSEXP );
        SEXP __result = target_r__get_pars(target);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_r__set_pars
void target_r__set_pars(Rcpp::XPtr<rodeint::target_r> target, SEXP pars);
RcppExport SEXP rodeint_target_r__set_pars(SEXP targetSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_r> >::type target(targetSEXP );
        Rcpp::traits::input_parameter< SEXP >::type pars(parsSEXP );
        target_r__set_pars(target, pars);
    }
    return R_NilValue;
END_RCPP
}
// test_harmonic_oscillator
rodeint::target_cpp test_harmonic_oscillator();
RcppExport SEXP rodeint_test_harmonic_oscillator() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        rodeint::target_cpp __result = test_harmonic_oscillator();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
