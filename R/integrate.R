## NOTE: There's a bit of a hassle here because Rcpp attributes builds
## the R functions for us -- but I want to do a tiny bit of checking
## and pass in different classes than it's expecting.  I've exported
## the functions as 'r_<foo>'.  However, it'd actually be very easy to
## just skip the exported functions entirely and use the .Call in
## these functions.  We will need to update any time the signatures
## update anyway.

##' Integrate a system of ordinary differential equations.  This is
##' just a convenience function (in odeint) and probably should not be
##' the final function used.
##'
##' @title Integrate an ODE System
##' @param target The target system, created by \code{\link{target_r}}
##' @param y Initial conditions
##' @param t0 Time to start the integration
##' @param t1 Time to finish the integration (not sure if we hit this
##' one exactly)
##' @param dt Step size
##' @param save_state Return information about intermediate points as
##' @author Rich FitzJohn
##' @export
integrate_simple <- function(target, y, t0, t1, dt,
                             save_state=FALSE) {
  assert_target_r(target)
  r_integrate_simple(target$ptr, y, t0, t1, dt, save_state)
}

##' Integrate a system of ODEs adaptively.
##'
##' @title Adaptively Integrate an ODE System
##' @param stepper A \code{controlled_stepper} object, created by
##' \code{\link{controlled_stepper}} (other types will be supported
##' soon).
##' @param target The target system, created by \code{\link{target_r}}
##' @param y Initial conditions
##' @param t0 Time to start the integration
##' @param t1 Time to finish the integration
##' @param dt Initial step size (will be tuned for controlled steppers
##' -- see odeint documentation)
##' @param save_state Return information about intermediate points as
##' an attribute?
##' @author Rich FitzJohn
##' @export
integrate_adaptive <- function(stepper, target, y, t0, t1, dt,
                               save_state=FALSE) {
  assert_controlled_stepper(stepper)
  assert_target_r(target)
  r_integrate_adaptive(stepper$ptr, target$ptr, y, t0, t1, dt, save_state)
}

##' Integrate a system of ODEs at fixed times (perhaps adaptively).
##' This version \emph{always} saves state -- that is the reason for
##' calling it.
##'
##' @title Adaptively Integrate an ODE System
##' @param stepper A \code{controlled_stepper} object, created by
##' \code{\link{controlled_stepper}} (other types will be supported
##' soon).
##' @param target The target system, created by \code{\link{target_r}}
##' @param y Initial conditions
##' @param times Vector of times.  First time is start time, last time
##' is end time.
##' @param dt Initial step size (will be tuned for controlled steppers
##' -- see odeint documentation)
##' @param save_state Return information about intermediate points as
##' an attribute?
##' @author Rich FitzJohn
##' @export
integrate_times <- function(stepper, target, y, times, dt) {
  assert_controlled_stepper(stepper)
  assert_target_r(target)
  if (length(times) < 2) {
    stop("Must provide at least two times")
  }
  r_integrate_times(stepper$ptr, target$ptr, y, times, dt)
}
