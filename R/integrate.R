## NOTE: There's a bit of a hassle here because Rcpp attributes builds
## the R functions for us -- but I want to do a tiny bit of checking
## and pass in different classes than it's expecting.  I've exported
## the functions as 'r_<foo>'.  However, it'd actually be very easy to
## just skip the exported functions entirely and use the .Call in
## these functions.  We will need to update any time the signatures
## update anyway.

##' Integrate a system of ODEs, taking fixed steps
##'
##' @title Adaptively Integrate an ODE System
##' @param stepper A \code{stepper} object, created by
##' \code{\link{stepper}} (other types will be supported
##' soon).
##' @param target The target system, created by \code{\link{target_r}}
##' @param y Initial conditions
##' @param t0 Time to start the integration
##' @param t1 Time to finish the integration
##' @param dt Step size
##' @param save_state Return information about intermediate points as
##' an attribute?
##' @author Rich FitzJohn
##' @export
integrate_const <- function(stepper, target, y, t0, t1, dt,
                            save_state=FALSE) {
  assert_stepper(stepper)
  target$integrate_const(stepper, y, t0, t1, dt, save_state)
  ## Alternatively:
  ## if (inherits(target, "target_cpp")) {
  ##   r_integrate_const_cpp(stepper$ptr, target$ptr, y, t0, t1, dt, save_state)
  ## } else if (inherits(target, "target_r")) {
  ##   r_integrate_const_cpp(stepper$ptr, target$ptr, y, t0, t1, dt, save_state)
  ## } else {
  ##   stop("Invalid target")
  ## }
}

##' Integrate a system of ODEs, taking a fixed number of fixed size steps.
##'
##' @title Adaptively Integrate an ODE System
##' @param stepper A \code{stepper} object, created by
##' \code{\link{stepper}} (other types will be supported
##' soon).
##' @param target The target system, created by \code{\link{target_r}}
##' @param y Initial conditions
##' @param t0 Time to start the integration
##' @param dt Step size
##' @param n Number of steps
##' @param save_state Return information about intermediate points as
##' an attribute?
##' @author Rich FitzJohn
##' @export
integrate_n_steps <- function(stepper, target, y, t0, dt, n,
                              save_state=FALSE) {
  assert_stepper(stepper)
  target$integrate_n_steps(stepper, y, t0, dt, n, save_state)
}

##' Integrate a system of ODEs adaptively.
##'
##' @title Adaptively Integrate an ODE System
##' @param stepper A \code{stepper} object, created by
##' \code{\link{stepper}} (other types will be supported
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
  assert_stepper(stepper)
  target$integrate_adaptive(stepper, y, t0, t1, dt, save_state)
}

##' Integrate a system of ODEs at fixed times (perhaps adaptively).
##' This version \emph{always} saves state -- that is the reason for
##' calling it.
##'
##' @title Adaptively Integrate an ODE System
##' @param stepper A \code{stepper} object, created by
##' \code{\link{stepper}} (other types will be supported
##' soon).
##' @param target The target system, created by \code{\link{target_r}}
##' @param y Initial conditions
##' @param times Vector of times.  First time is start time, last time
##' is end time.
##' @param dt Initial step size (will be tuned for controlled steppers
##' -- see odeint documentation)
##' @author Rich FitzJohn
##' @export
integrate_times <- function(stepper, target, y, times, dt) {
  assert_stepper(stepper)
  if (length(times) < 2) {
    stop("Must provide at least two times")
  }
  target$integrate_times(stepper, y, times, dt)
}

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
  target$integrate_simple(y, t0, t1, dt, save_state)
}
