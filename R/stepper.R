##' Stepper (documentation coming)
##' @title Stepper
##' @aliases stepper
##' @rdname stepper
##' @export stepper
##' @export
stepper <- setRefClass("stepper",
                       fields=list(
                         category="character",
                         type="character",
                         stiff_state="logical",
                         abs_tol="numeric",
                         rel_tol="numeric",
                         ptr="externalptr"))
stepper$lock(names(stepper$fields()))

stepper$methods(initialize=function(category, type, stiff_state,
                  abs_tol, rel_tol) {
  category <<- category
  type     <<- type
  stiff_state <<- stiff_state
  abs_tol <<- abs_tol
  rel_tol <<- rel_tol
  ptr <<- stepper__ctor(category, type, stiff_state,
                        abs_tol, rel_tol)
})

## This is going to change at some point, but this is a list of the
## possible controlled stepper types.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{stepper}}.
##' @title Types of Controlled Stepper
##' @author Rich FitzJohn
##' @export
##' @rdname stepper_types
##' @param category Either "basic" or "controlled"
##' @param have_jacobian Logical indicating if the stepper will work
##' on problems with a Jacobian (this is a superset: every problem
##' that provides a Jacobian will work fine with steppers that don't
##' require a Jacobian).
stepper_types <- function(category, have_jacobian=FALSE) {
  switch(category,
         basic=stepper_basic_types(have_jacobian),
         controlled=stepper_controlled_types(have_jacobian),
         stop("Invalid stepper category"))
}

##' @export
##' @rdname stepper_types
stepper_basic_types <- function(have_jacobian=FALSE) {
  c("euler",
    "modified_midpoint",
    "runge_kutta4",
    "runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5",
    if (have_jacobian) "rosenbrock4")
}

##' @export
##' @rdname stepper_types
stepper_controlled_types <- function(have_jacobian=FALSE) {
  c("runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5",
    if(have_jacobian) "rosenbrock4")
}

##' @export
##' @rdname stepper_types
stepper_categories <- function() {
  c("basic", "controlled")
}

##' @rdname stepper
##' @export
make_stepper_basic <- function(type, atol=NA_real_, rtol=NA_real_,
                               stiff_state=NA) {
  if (!is.na(atol) || !is.na(rtol)) {
    warning("Ignoring provided tolerance arguments")
  }
  if (is.na(stiff_state)) {
    stiff_state <- type == "rosenbrock4"
  }
  stepper("basic", type, stiff_state, NA_real_, NA_real_)
}

## TODO: See the comment about stiff_state below.
##' @rdname stepper
##' @export
##' @param atol Absolute tolerance (see odeint docs for now)
##' @param rtol Relative tolerance (see odeint docs for now)
##' @param stiff_state Logical, indicating on whether the stepper
##' should use the internal data structures required by the stiff
##' systems.  This is likely to disappear soon, as it depends entirely
##' on the system itself.  The default, NA, will switch based on the
##' \code{type} argument -- \code{rosenbrock4} is the stiff system
##' stepper.  However, to use other steppers with the stiff system, we
##' need to set up normal steppers similarly.  So probably at runtime
##' this will just rebuild the stepper for us.
make_stepper_controlled <- function(type, atol=1e-6, rtol=1e-6,
                                    stiff_state=NA) {
  if (is.na(stiff_state)) {
    stiff_state <- type == "rosenbrock4"
  }
  stepper("controlled", type, stiff_state, atol, rtol)
}

##' @rdname stepper
##' @export
##' @param category Either "basic" or "controlled"
##' @param type The type of stepper (e.g. "runge_kutta4")
##' @param ... Additional parameters passed from \code{make_stepper}
##' to either \code{make_stepper_basic} (none allowed) or
##' \code{make_stepper_controlled}.
make_stepper <- function(category, type, ...) {
  make <- switch(category,
                 basic=make_stepper_basic,
                 controlled=make_stepper_controlled,
                 stop("Invalid stepper category"))
  make(type, ...)
}
