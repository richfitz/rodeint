##' Stepper (documentation coming)
##' @title Stepper
##' @aliases stepper
##' @rdname stepper
##' @export
stepper <- setRefClass("stepper",
                       fields=list(
                         category="character",
                         type="character",
                         ublas_state="logical",
                         abs_tol="numeric",
                         rel_tol="numeric",
                         ptr="externalptr"))
stepper$lock(names(stepper$fields()))

stepper$methods(show = function(details=FALSE) {
  cat("A stepper for solving ordinary differential equations\n\n")
  cat("This object has no useful methods\n")
  cat("Pass this stepper to functions in ?rodeint_integrate\n")
  cat("\nDetails:\n")
  cat(sprintf("\tcategory: %s\n", category))
  cat(sprintf("\talgorithm: %s\n", type))
  if (category != "basic") {
    cat(sprintf("\tabs_tol: %s\n", abs_tol))
    cat(sprintf("\trel_tol: %s\n", rel_tol))
  }
  if (details) {
    cat("----------------------------\n")
    cat(sprintf("addr:  %s\n", ptr_address(ptr)))
    cat(sprintf("ublas_state: %s\n",
                if (ublas_state) "yes" else "no"))
    cat("----------------------------\n")
  }
})

stepper$methods(initialize=function(category, type, ublas_state,
                  abs_tol, rel_tol) {
  category <<- category
  type     <<- type
  ublas_state <<- ublas_state
  abs_tol <<- abs_tol
  rel_tol <<- rel_tol
  ptr <<- stepper__ctor(category, type, ublas_state,
                        abs_tol, rel_tol)
})

stepper$methods(details = function() {
  stepper__type(ptr)
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
make_stepper_basic <- function(type, abs_tol=NA_real_, rel_tol=NA_real_,
                               ublas_state=NA) {
  if (!is.na(abs_tol) || !is.na(rel_tol)) {
    warning("Ignoring provided tolerance arguments")
  }
  if (is.na(ublas_state)) {
    ublas_state <- type == "rosenbrock4"
  }
  stepper("basic", type, ublas_state, NA_real_, NA_real_)
}

## TODO: See the comment about ublas_state below.
##' @rdname stepper
##' @export
##' @param abs_tol Absolute tolerance (see odeint docs for now)
##' @param rel_tol Relative tolerance (see odeint docs for now)
##' @param ublas_state Logical, indicating on whether the stepper
##' should use the internal data structures required by the stiff
##' systems.  This is likely to disappear soon, as it depends entirely
##' on the system itself.  The default, NA, will switch based on the
##' \code{type} argument -- \code{rosenbrock4} is the stiff system
##' stepper.  However, to use other steppers with the stiff system, we
##' need to set up normal steppers similarly.  So probably at runtime
##' this will just rebuild the stepper for us.
make_stepper_controlled <- function(type, abs_tol=1e-6, rel_tol=1e-6,
                                    ublas_state=NA) {
  if (is.na(ublas_state)) {
    ublas_state <- type == "rosenbrock4"
  }
  stepper("controlled", type, ublas_state, abs_tol, rel_tol)
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
