// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rodeint.h"
#include <Rcpp.h>

using namespace Rcpp;

// harmonic_oscillator_basic
std::vector<double> harmonic_oscillator_basic(std::vector<double> y, double t0, double t1, double par, double dt);
RcppExport SEXP rodeint_harmonic_oscillator_basic(SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP parSEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::vector<double> >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type par(parSEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        std::vector<double> __result = harmonic_oscillator_basic(y, t0, t1, par, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrator_make
rodeint::integrator integrator_make(rodeint::ode_target_r target);
RcppExport SEXP rodeint_integrator_make(SEXP targetSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::ode_target_r >::type target(targetSEXP );
        rodeint::integrator __result = integrator_make(target);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrator_derivs
rodeint::integrator::state_type integrator_derivs(Rcpp::XPtr<rodeint::integrator> obj, const rodeint::integrator::state_type& y, const double t);
RcppExport SEXP rodeint_integrator_derivs(SEXP objSEXP, SEXP ySEXP, SEXP tSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::integrator> >::type obj(objSEXP );
        Rcpp::traits::input_parameter< const rodeint::integrator::state_type& >::type y(ySEXP );
        Rcpp::traits::input_parameter< const double >::type t(tSEXP );
        rodeint::integrator::state_type __result = integrator_derivs(obj, y, t);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// integrator_integrate
rodeint::integrator::state_type integrator_integrate(Rcpp::XPtr<rodeint::integrator> obj, rodeint::integrator::state_type y, double t0, double t1, double dt);
RcppExport SEXP rodeint_integrator_integrate(SEXP objSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::integrator> >::type obj(objSEXP );
        Rcpp::traits::input_parameter< rodeint::integrator::state_type >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        rodeint::integrator::state_type __result = integrator_integrate(obj, y, t0, t1, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// ode_target_r_make
rodeint::ode_target_r ode_target_r_make(Rcpp::Function derivs, SEXP pars_type);
RcppExport SEXP rodeint_ode_target_r_make(SEXP derivsSEXP, SEXP pars_typeSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::Function >::type derivs(derivsSEXP );
        Rcpp::traits::input_parameter< SEXP >::type pars_type(pars_typeSEXP );
        rodeint::ode_target_r __result = ode_target_r_make(derivs, pars_type);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// ode_target_r_derivs
std::vector<double> ode_target_r_derivs(Rcpp::XPtr<rodeint::ode_target_r> obj, std::vector<double> y, double t);
RcppExport SEXP rodeint_ode_target_r_derivs(SEXP objSEXP, SEXP ySEXP, SEXP tSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::ode_target_r> >::type obj(objSEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t(tSEXP );
        std::vector<double> __result = ode_target_r_derivs(obj, y, t);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// ode_target_r_set_pars
void ode_target_r_set_pars(Rcpp::XPtr<rodeint::ode_target_r> obj, SEXP pars);
RcppExport SEXP rodeint_ode_target_r_set_pars(SEXP objSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::ode_target_r> >::type obj(objSEXP );
        Rcpp::traits::input_parameter< SEXP >::type pars(parsSEXP );
        ode_target_r_set_pars(obj, pars);
    }
    return R_NilValue;
END_RCPP
}
// ode_target_r_basic
std::vector<double> ode_target_r_basic(rodeint::ode_target_r obj, std::vector<double> y, double t0, double t1, double dt);
RcppExport SEXP rodeint_ode_target_r_basic(SEXP objSEXP, SEXP ySEXP, SEXP t0SEXP, SEXP t1SEXP, SEXP dtSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< rodeint::ode_target_r >::type obj(objSEXP );
        Rcpp::traits::input_parameter< std::vector<double> >::type y(ySEXP );
        Rcpp::traits::input_parameter< double >::type t0(t0SEXP );
        Rcpp::traits::input_parameter< double >::type t1(t1SEXP );
        Rcpp::traits::input_parameter< double >::type dt(dtSEXP );
        std::vector<double> __result = ode_target_r_basic(obj, y, t0, t1, dt);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// make_foo
rodeint::foo make_foo(double a, double b);
RcppExport SEXP rodeint_make_foo(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type a(aSEXP );
        Rcpp::traits::input_parameter< double >::type b(bSEXP );
        rodeint::foo __result = make_foo(a, b);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// foo_run
double foo_run(Rcpp::XPtr<rodeint::foo> obj);
RcppExport SEXP rodeint_foo_run(SEXP objSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::foo> >::type obj(objSEXP );
        double __result = foo_run(obj);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// foo_set_a
void foo_set_a(Rcpp::XPtr<rodeint::foo> obj, double a);
RcppExport SEXP rodeint_foo_set_a(SEXP objSEXP, SEXP aSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::XPtr<rodeint::foo> >::type obj(objSEXP );
        Rcpp::traits::input_parameter< double >::type a(aSEXP );
        foo_set_a(obj, a);
    }
    return R_NilValue;
END_RCPP
}
// foo_run_copy
double foo_run_copy(const rodeint::foo& obj);
RcppExport SEXP rodeint_foo_run_copy(SEXP objSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const rodeint::foo& >::type obj(objSEXP );
        double __result = foo_run_copy(obj);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
