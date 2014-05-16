##' Stepper (documentation coming)
##' @title Stepper (deSolve wrappers)
##' @aliases stepper_deSolve
##' @rdname stepper_deSolve
##' @export
stepper_deSolve <- setRefClass("stepper_deSolve",
                               fields=list(
                                 category="character",
                                 type="character",
                                 abs_tol="numeric",
                                 rel_tol="numeric",
                                 method="list"))
stepper_deSolve$lock(names(stepper_deSolve$fields()))

## TODO: When implementing lsoda support, list might not be OK here.

stepper_deSolve$methods(initialize=
                        function(category, type, abs_tol, rel_tol, method) {
  category <<- category
  type <<- type
  abs_tol <<- abs_tol
  rel_tol <<- rel_tol
  method <<- method
})

##' @rdname stepper_deSolve
##' @export
##' @param category Either "basic" or "controlled"
##' @param type The type of stepper (e.g. "runge_kutta4")
##' @param ... Additional parameters passed from \code{make_stepper}
##' to either \code{make_stepper_basic} (none allowed) or
##' \code{make_stepper_controlled}.
make_stepper_deSolve <- function(category, type, ...) {
  make <- switch(category,
                 basic=make_stepper_deSolve_basic,
                 controlled=make_stepper_deSolve_controlled,
                 stop("Invalid stepper category"))
  make(type, ...)
}

##' @rdname stepper_deSolve
##' @export
make_stepper_deSolve_basic <- function(type) {
  rkMethod <- deSolve::rkMethod
  method <- switch(type,
                   euler=rkMethod("euler"),
                   modified_midpoint=rkMethod("rk2"), # I *think*
                   runge_kutta4=rkMethod("rk4"),
                   runge_kutta_cash_karp54=rkMethod("rk45ck"),
                   runge_kutta_fehlberg78=rkMethod("rk78f"),
                   runge_kutta_dopri5=rkMethod("rk45dp7"),
                   stop("Unknown type: ", type))
  abs_tol <- rel_tol <- 1
  stepper_deSolve("basic", type, abs_tol, rel_tol, method)
}

##' @rdname stepper_deSolve
##' @export
##' @param abs_tol Absolute tolerance (see atol in deSolve docs for now)
##' @param rel_tol Relative tolerance (see rtol in deSolve docs for now)
make_stepper_deSolve_controlled <- function(type, abs_tol=1e-6, rel_tol=1e-6) {
  ## TODO: Support other types that are in deSolve?  We are definitely
  ## going to want lsoda in here at some point.
  rkMethod <- deSolve::rkMethod
  method <- switch(type,
                   runge_kutta_cash_karp54=rkMethod("rk45ck"),
                   runge_kutta_fehlberg78=rkMethod("rk78f"),
                   runge_kutta_dopri5=rkMethod("rk45dp7"),
                   stop("Unknown type: ", type))
  stepper_deSolve("controlled", type, abs_tol, rel_tol, method)
}
