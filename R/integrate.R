## TODO: Document these all on one page?  Lots of duplicated
## documentation otherwise.

## NOTE: These functions also form the basis of *generating* functions
## we could have another set of functions
##   make_integrate_const(stepper, target, y, t0, t1, dt, save_state)
## that could set all provided arguments as defaults, arrange for
## checking of the remainder and return a function.  That means we can
## really easily turn a system of ODEs into a set of equations!
##
## Lifting the checking from within the function to the generator
## would be harder though, unless it *always* runs through the
## generator.

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
  ## This is a bit insane, but needs to be done or error messages from
  ## the underlying rodeint_integrate function are indecipherable.
  assert_stepper(stepper)
  assert_target(target)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(t1)
  assert_scalar_numeric(dt)
  target$odeint_integrate_const(stepper$ptr, target$ptr, y, t0, t1, dt,
                                save_state)
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
  assert_target(target)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(dt)
  assert_scalar_size(n)
  target$odeint_integrate_n_steps(stepper$ptr, target$ptr, y, t0, dt, n,
                                  save_state)
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
  assert_target(target)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(t1)
  assert_scalar_numeric(dt)
  target$odeint_integrate_adaptive(stepper$ptr, target$ptr, y, t0, t1, dt,
                                   save_state)
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
  assert_target(target)
  assert_numeric(y)
  assert_numeric(times) # TODO: check sorted
  if (length(times) < 2) {
    stop("Must provide at least two times")
  }
  assert_scalar_numeric(dt)
  target$odeint_integrate_times(stepper$ptr, target$ptr, y, times, dt)
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
  assert_target(target)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(t1)
  assert_scalar_numeric(dt)
  target$odeint_integrate_simple(target$ptr, y, t0, t1, dt,
                                 save_state)
}

##' Helper function for binding targets, steppers and integration
##' functions together.  This can be used to create a function
##' \code{f(y0, t)} from your system of \code{f\'(y0, t)}.
##'
##' Note the opposite ordering of the \code{target} and \code{stepper}
##' arguments here compared with the rest of the package (following
##' \code{odeint}.
##'
##' @title Make Integration Function
##' @param target A target function (\code{\link{target_r}},
##' \code{\link{target_cpp}} or \code{\link{target_class}}).
##' @param ... Additional arguments to bind.  Setting \code{t0} when a
##' system is time independent means \code{t1} will be a function of
##' elapsed time, which can be useful.  All integrate functions take a
##' \code{dt} argument, so that's useful to bind too.  You can also
##' pass in \code{set_as_defaults=TRUE} and the arguments, including
##' \code{stepper} and \code{target} will simply be set as defaults
##' allowing some tuning later.
##' @param stepper A stepper object. By default the controlled
##' \code{runge_kutta_dopri} stepper is used with default tolerances.
##' If you want to change the tolerance, you must provide a different
##' stepper object.
##' @param integrate One of the integration functions.  The default is
##' \code{\link{integrate_adaptive}}.
##' @author Rich FitzJohn
##' @export
make_integrate <- function(target, ..., stepper=NULL,
                           integrate=integrate_adaptive) {
  if (is.null(stepper)) {
    stepper <- make_stepper_controlled("runge_kutta_dopri5")
  }
  assert_stepper(stepper)
  assert_target(target)
  target <- target$copy()
  ## TODO: Option to rewrite t1 -> t if t0 = 0?
  partially_apply(integrate, stepper=stepper, target=target, ...)
}

## Possible inefficiencies here -- the construction of the target in
## the first place, the extra copy that happens during all
## make_integrate calls here.  I'm not concerned about it though - the
## copy should be fairly cheap and the startup cost should not be bad
## either.
##' @rdname make_integrate
##' @export
make_integrate_pars <- function(target, ...) {
  ## First check that things work with the arguments we got (or we
  ## might not find out for ages).  This also deals with the issues
  ## described in ?force
  make_integrate(target, ...)
  target <- target$copy()
  function(pars) {
    target$set_pars(pars)
    make_integrate(target, ...)
  }
}
