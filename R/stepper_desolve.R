##' Stepper (documentation coming)
##' @title Stepper (deSolve wrappers)
##' @aliases stepper_deSolve
##' @rdname stepper_deSolve
##' @export stepper_deSolve
##' @export
stepper_deSolve <- setRefClass("stepper_deSolve",
                               fields=list(
                                 category="character",
                                 type="character",
                                 atol="numeric",
                                 rtol="numeric",
                                 method="list"))
stepper_deSolve$lock(c("category", "type", "atol", "rtol", "method"))

## TODO: When implementing lsoda support, list might not be OK here.

stepper_deSolve$methods(initialize=
                        function(category, type, atol, rtol, method) {
  category <<- category
  type <<- type
  atol <<- atol
  rtol <<- rtol
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
  atol <- rtol <- 1
  stepper_deSolve("basic", type, atol, rtol, method)
}

##' @rdname stepper_deSolve
##' @export
##' @param atol Absolute tolerance (see deSolve docs for now)
##' @param rtol Relative tolerance (see deSolve docs for now)
make_stepper_deSolve_controlled <- function(type, atol=1e-6, rtol=1e-6) {
  ## TODO: Support other types that are in deSolve?  We are definitely
  ## going to want lsoda in here at some point.
  rkMethod <- deSolve::rkMethod
  method <- switch(type,
                   runge_kutta_cash_karp54=rkMethod("rk45ck"),
                   runge_kutta_fehlberg78=rkMethod("rk78f"),
                   runge_kutta_dopri5=rkMethod("rk45dp7"),
                   stop("Unknown type: ", type))
  stepper_deSolve("controlled", type, atol, rtol, method)
}
