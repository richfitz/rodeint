// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rodeint.h"
#include <Rcpp.h>

using namespace Rcpp;

// controlled_stepper__ctor
rodeint::controlled_stepper controlled_stepper__ctor(std::string type, double eps_abs, double eps_rel);
RcppExport SEXP rodeint_controlled_stepper__ctor(SEXP typeSEXP, SEXP eps_absSEXP, SEXP eps_relSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type type(typeSEXP );
        Rcpp::traits::input_parameter< double >::type eps_abs(eps_absSEXP );
        Rcpp::traits::input_parameter< double >::type eps_rel(eps_relSEXP );
        rodeint::controlled_stepper __result = controlled_stepper__ctor(type, eps_abs, eps_rel);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// controlled_stepper__type
std::string controlled_stepper__type(rodeint::controlled_stepper s);
RcppExport SEXP rodeint_controlled_stepper__type(SEXP sSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::controlled_stepper >::type s(sSEXP );
        std::string __result = controlled_stepper__type(s);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate
Rcpp::NumericVector r_integrate(rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_r_integrate(SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
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
        Rcpp::NumericVector __result = r_integrate(target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// r_integrate_adaptive
Rcpp::NumericVector r_integrate_adaptive(rodeint::controlled_stepper stepper, rodeint::target_r target, rodeint::target_r::state_type y, double t0, double t1, double dt, bool save_state = false);
RcppExport SEXP rodeint_r_integrate_adaptive(SEXP stepperSEXP, SEXP targetSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP, SEXP save_stateSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::controlled_stepper >::type stepper(stepperSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r >::type target(targetSEXP );
        Rcpp::traits::input_parameter< rodeint::target_r::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        Rcpp::traits::input_parameter< bool >::type save_state(save_stateSEXP );
        Rcpp::NumericVector __result = r_integrate_adaptive(stepper, target, y, t0, t1, dt, save_state);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
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
