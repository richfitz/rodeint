// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rodeint.h"
#include <Rcpp.h>

using namespace Rcpp;

// integrate_const_r
Rcpp::NumericVector integrate_const_r(rodeint::stepper stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_integrate_const_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_const_r(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_const_cpp
Rcpp::NumericVector integrate_const_cpp(rodeint::stepper stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_integrate_const_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_const_cpp(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_const_class
Rcpp::NumericVector integrate_const_class(rodeint::stepper stepper, rodeint::target_class target, rodeint::target_class::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_integrate_const_class(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_const_class(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_n_steps_r
Rcpp::NumericVector integrate_n_steps_r(rodeint::stepper stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double dt, int n, bool save_state);
RcppExport SEXP rodeint_integrate_n_steps_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP dtSEXP, SEXP nSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< int >::type n(nSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_n_steps_r(stepper, target, y, t0, dt, n, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_n_steps_cpp
Rcpp::NumericVector integrate_n_steps_cpp(rodeint::stepper stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double dt, int n, bool save_state);
RcppExport SEXP rodeint_integrate_n_steps_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP dtSEXP, SEXP nSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< int >::type n(nSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_n_steps_cpp(stepper, target, y, t0, dt, n, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_n_steps_class
Rcpp::NumericVector integrate_n_steps_class(rodeint::stepper stepper, rodeint::target_class target, rodeint::target_class::state_type y, double t0, double dt, int n, bool save_state);
RcppExport SEXP rodeint_integrate_n_steps_class(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP dtSEXP, SEXP nSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< int >::type n(nSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_n_steps_class(stepper, target, y, t0, dt, n, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_adaptive_r
Rcpp::NumericVector integrate_adaptive_r(rodeint::stepper stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_integrate_adaptive_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_adaptive_r(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_adaptive_cpp
Rcpp::NumericVector integrate_adaptive_cpp(rodeint::stepper stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_integrate_adaptive_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_adaptive_cpp(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_adaptive_class
Rcpp::NumericVector integrate_adaptive_class(rodeint::stepper stepper, rodeint::target_class target, rodeint::target_class::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_integrate_adaptive_class(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_adaptive_class(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_times_r
Rcpp::NumericMatrix integrate_times_r(rodeint::stepper stepper, rodeint::target_r target, rodeint::target_r::state_type y, std::vector<double> times, double dt);
RcppExport SEXP rodeint_integrate_times_r(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP timesSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type times(timesSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::NumericMatrix __result = integrate_times_r(stepper, target, y, times, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_times_cpp
Rcpp::NumericVector integrate_times_cpp(rodeint::stepper stepper, rodeint::target_cpp target, rodeint::target_cpp::state_type y, std::vector<double> times, double dt);
RcppExport SEXP rodeint_integrate_times_cpp(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP timesSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_cpp::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type times(timesSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::NumericVector __result = integrate_times_cpp(stepper, target, y, times, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_times_class
Rcpp::NumericVector integrate_times_class(rodeint::stepper stepper, rodeint::target_class target, rodeint::target_class::state_type y, std::vector<double> times, double dt);
RcppExport SEXP rodeint_integrate_times_class(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP timesSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type times(timesSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::NumericVector __result = integrate_times_class(stepper, target, y, times, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_simple_r
Rcpp::NumericVector integrate_simple_r(rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_integrate_simple_r(SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
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
        Rcpp::NumericVector __result = integrate_simple_r(target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_simple_cpp
Rcpp::NumericVector integrate_simple_cpp(rodeint::target_cpp target, rodeint::target_cpp::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_integrate_simple_cpp(SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
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
        Rcpp::NumericVector __result = integrate_simple_cpp(target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrate_simple_class
Rcpp::NumericVector integrate_simple_class(rodeint::target_class target, rodeint::target_class::state_type y, double t0, double t1, double dt, bool save_state);
RcppExport SEXP rodeint_integrate_simple_class(SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::target_class >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = integrate_simple_class(target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stepper_controlled__ctor
rodeint::stepper stepper_controlled__ctor(std::string type, double eps_abs, double eps_rel);
RcppExport SEXP rodeint_stepper_controlled__ctor(SEXP typeSEXP, SEXP eps_absSEXP, SEXP eps_relSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type type(typeSEXP );
        Rcpp::traits::input_parameter< double >::type eps_abs(eps_absSEXP );
        Rcpp::traits::input_parameter< double >::type eps_rel(eps_relSEXP );
        rodeint::stepper __result = stepper_controlled__ctor(type, eps_abs, eps_rel);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stepper_basic__ctor
rodeint::stepper stepper_basic__ctor(std::string type);
RcppExport SEXP rodeint_stepper_basic__ctor(SEXP typeSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type type(typeSEXP );
        rodeint::stepper __result = stepper_basic__ctor(type);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stepper__type
std::vector<std::string> stepper__type(rodeint::stepper s);
RcppExport SEXP rodeint_stepper__type(SEXP sSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::stepper >::type s(sSEXP );
        std::vector<std::string> __result = stepper__type(s);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_class__get_pars
rodeint::target_class::pars_type target_class__get_pars(Rcpp::XPtr<rodeint::target_class> target);
RcppExport SEXP rodeint_target_class__get_pars(SEXP targetSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_class> >::type target(targetSEXP );
        rodeint::target_class::pars_type __result = target_class__get_pars(target);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// target_class__set_pars
void target_class__set_pars(Rcpp::XPtr<rodeint::target_class> target, rodeint::target_class::pars_type pars);
RcppExport SEXP rodeint_target_class__set_pars(SEXP targetSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_class> >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::pars_type >::type pars(parsSEXP );
        target_class__set_pars(target, pars);
    }
    return R_NilValue;
END_RCPP
}
// target_class__derivs
rodeint::target_class::state_type target_class__derivs(Rcpp::XPtr<rodeint::target_class> target, rodeint::target_class::state_type y, double t);
RcppExport SEXP rodeint_target_class__derivs(SEXP targetSEXP, SEXP ySEXP, SEXP tSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::target_class> >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_class::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t(tSEXP );
        rodeint::target_class::state_type __result = target_class__derivs(target, y, t);
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
// test_harmonic_oscillator_cpp
rodeint::target_cpp test_harmonic_oscillator_cpp(std::vector<double> pars);
RcppExport SEXP rodeint_test_harmonic_oscillator_cpp(SEXP parsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::vector<double> >::type pars(parsSEXP );
        rodeint::target_cpp __result = test_harmonic_oscillator_cpp(pars);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// test_harmonic_oscillator_class
rodeint::target_class test_harmonic_oscillator_class(double pars);
RcppExport SEXP rodeint_test_harmonic_oscillator_class(SEXP parsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type pars(parsSEXP );
        rodeint::target_class __result = test_harmonic_oscillator_class(pars);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
