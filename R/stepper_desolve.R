##' Stepper (documentation coming)
##' @title Stepper (deSolve wrappers)
##' @aliases stepper_deSolve
##' @rdname stepper_deSolve
##' @export
stepper_deSolve <- setRefClass("stepper_deSolve",
                               fields=list(
                                 category="character",
                                 algorithm="character",
                                 abs_tol="numeric",
                                 rel_tol="numeric",
                                 method="list"))
stepper_deSolve$lock(names(stepper_deSolve$fields()))

## TODO: When implementing lsoda support, list might not be OK here.

stepper_deSolve$methods(initialize=
                        function(category, algorithm,
                                 abs_tol, rel_tol, method) {
  category <<- category
  algorithm <<- algorithm
  abs_tol <<- abs_tol
  rel_tol <<- rel_tol
  method <<- method
})

##' @rdname stepper_deSolve
##' @export
##' @param category Either "basic" or "controlled"
##' @param algorithm The stepper algorithm (e.g. "runge_kutta4")
##' @param ... Additional parameters passed from \code{make_stepper}
##' to either \code{make_stepper_basic} (none allowed) or
##' \code{make_stepper_controlled}.
make_stepper_deSolve <- function(category, algorithm, ...) {
  make <- switch(category,
                 basic=make_stepper_deSolve_basic,
                 controlled=make_stepper_deSolve_controlled,
                 stop("Invalid stepper category"))
  make(algorithm, ...)
}

##' @rdname stepper_deSolve
##' @export
make_stepper_deSolve_basic <- function(algorithm) {
  rkMethod <- deSolve::rkMethod
  method <- switch(algorithm,
                   euler=rkMethod("euler"),
                   modified_midpoint=rkMethod("rk2"), # I *think*
                   runge_kutta4=rkMethod("rk4"),
                   runge_kutta_cash_karp54=rkMethod("rk45ck"),
                   runge_kutta_fehlberg78=rkMethod("rk78f"),
                   runge_kutta_dopri5=rkMethod("rk45dp7"),
                   stop("Unknown algorithm: ", algorithm))
  abs_tol <- rel_tol <- 1
  stepper_deSolve("basic", algorithm, abs_tol, rel_tol, method)
}

##' @rdname stepper_deSolve
##' @export
##' @param abs_tol Absolute tolerance (see atol in deSolve docs for now)
##' @param rel_tol Relative tolerance (see rtol in deSolve docs for now)
make_stepper_deSolve_controlled <- function(algorithm,
                                            abs_tol=1e-6, rel_tol=1e-6) {
  ## TODO: Support other algorithms that are in deSolve?  We are
  ## definitely going to want lsoda in here at some point.
  rkMethod <- deSolve::rkMethod
  method <- switch(algorithm,
                   runge_kutta_cash_karp54=rkMethod("rk45ck"),
                   runge_kutta_fehlberg78=rkMethod("rk78f"),
                   runge_kutta_dopri5=rkMethod("rk45dp7"),
                   stop("Unknown algorithm: ", algorithm))
  stepper_deSolve("controlled", algorithm, abs_tol, rel_tol, method)
}

## Proof of concept only
integrate_adaptive_deSolve <- function(stepper, ode_system, y,
                                       t0, t1, dt, save_state=FALSE) {
  assert_ode_system(ode_system)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(t1)
  assert_scalar_numeric(dt)
  if (save_state) {
    stop("save_state=TRUE not supported")
  }

  fakepars <- NULL
  hmin <- 0
  hmax <- NULL
  hini <- dt

  info <- ode_system$deSolve_info()
  ## Adaptive
  times <- c(t0, t1)
  res <- deSolve::ode(y, times, info$func, fakepars, # jacfunc=info$jacfunc,
                      hini=hini, hmin=hmin, hmax=hmax,
                      method=stepper$method,
                      atol=stepper$abs_tol, rtol=stepper$rel_tol,
                      initfunc=info$initfunc, initpar=info$initpar,
                      dllname=info$dllname)
  unname(res[-1,-1,drop=TRUE])
}
