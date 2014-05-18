##' Integrate a system of ODEs
##'
##' There are four different integration functions here.  See the
##' odeint documentation for more detail about the precice
##' differences.  The summary, though, is:
##'
##' \itemize{
##' \item \code{integrate_const} "Equidistant observer calls"
##' \item \code{integrate_n_steps} "Integrate a given number of steps"
##' \item \code{integrate_adaptive} "Observer calls at each step"
##' \item \code{integrate_times} "Observer calls at given time points"
##' }
##'
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/odeint_in_detail/integrate_functions.html}{This page}
##' provides much more detail. The functions here are direct wrappers
##' of the odeint functions, so the interpretation is identical.
##'
##' If you just want the end-points, \code{integrate_adaptive} is
##' probably the function to use.  If you want to know intermediate
##' values at particular times too, use \code{integrate_times}.  The
##' functions \code{integrate_const} and \code{integrate_n_steps} are
##' provided more for completeness than for utility, but they may be
##' useful in specific applications.
##'
##' @title Integrate a System of ODEs
##' @param stepper A \code{stepper} object, created by
##' \code{\link{make_stepper}}
##' @param ode_system The ode_system system, created by
##' \code{\link{ode_system}}
##' @param y Initial conditions
##' @param t0 Time to start the integration
##' @param t1 Time to finish the integration
##' @param dt Step size
##' @param n Number of steps to take (\code{integrate_n_steps} only)
##' @param times Vector of times (\code{integrate_times} only)
##' @param save_state Logical: return information about intermediate
##' points as an attribute.  Not applicable for
##' \code{integrate_times}.
##' @author Rich FitzJohn
##' @rdname rodeint_integrate
##' @seealso
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/concepts/dense_output_stepper.html}{dense
##' output steppers}
##' @examples
##' ## Picking up with the harmonic oscillator from the ode_system
##' ## example:
##' derivs <- function(y, t, pars) {
##'   c(y[2],
##'     -y[1] - pars * y[2])
##' }
##'
##' ## Parameters of the system:
##' pars <- 0.5
##'
##' ## Build the system itself
##' sys <- ode_system(derivs, pars)
##'
##' ## We also need a stepper (see the ?stepper help page)
##' s <- make_stepper("dense", "runge_kutta_dopri5")
##'
##' ## Suppose we want output at these times:
##' times <- seq(0, 10, length=101)
##'
##' ## Starting from this initial state:
##' y0 <- c(0, 1)
##'
##' ## Initial step size guess:
##' dt <- 0.01
##'
##' ## This intergrates the system (all integrate_ functions take stepper,
##' ## system, y as the first theree arguments).
##' y <- integrate_times(s, sys, y0, times, dt)
##'
##' ## Here are the variables changing over time:
##' matplot(times, y, type="l")
##'
##' ## The result also has useful attributes.  The "t" attribute is the
##' ## times (this is the same as 'times' here)
##' attr(y, "t")
##'
##' ## The "steps" attributes contains the number of steps the system took
##' attr(y, "steps")
##'
##' ## Here, the number is lower than the number of times!  This is
##' ## because we used a "dense output stepper" which can interpolate back
##' ## over times that have passed.  See the "See also" section for a link
##' ## to the odeint help about this.
##'
##' ## The "y" attribute contains the final system state - here it will be
##' ## the same as the last row of 'y' itself
##' attr(y, "y")
##' y[nrow(y),]
##'
##' ## If you don't care about intermediate times, the
##' ## "integrate_adaptive" function is probably the right one to use.
##' y_a <- integrate_adaptive(s, sys, y0, min(times), max(times), dt)
##' y_a
##'
##' ## Here we jump all the way to the final time as quickly as possible.
##' y_a
##' attr(y, "y")
##'
##' ## It's possible to remember the steps taken by usin the "save_state"
##' ## argument.
##' y_a <- integrate_adaptive(s, sys, y0, min(times), max(times), dt,
##'                           save_state=TRUE)
##'
##' ## Now y_a contains three attributes, the same as returned by
##' ## integrate_times.  "t" is the vector of times that the stepper
##' ## stopped at:
##' attr(y_a, "t")
##'
##' ## "y" contains the y values at these points, wherever they are:
##' attr(y_a, "y")
##'
##' ## Here is the dense output (thick lines) with the actual observed
##' ## spots overlaid.  You can see the step size change at the beginning
##' ## of the problem until the stepper works out how fast it can go while
##' ## achieving the required accuracy.
##' matplot(times, y, type="l", col=c("grey", "pink"), lty=1, lwd=10,
##'         xlim=c(0, 2))
##' matlines(attr(y_a, "t"), attr(y_a, "y"), type="o", pch=19)
##'
##' ## Manually specifying the stepper and step size and passing around 6
##' ## or so parameters all the time gets old, so there is a function
##' ## "make_integrate" to help collect everything together.  See
##' ## ?make_integrate for more information.
##'
##' @export
integrate_const <- function(stepper, ode_system, y, t0, t1, dt,
                            save_state=FALSE) {
  ## This is a bit insane, but needs to be done or error messages from
  ## the underlying rodeint_integrate function are indecipherable.
  assert_stepper(stepper)
  assert_ode_system(ode_system)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(t1)
  assert_scalar_numeric(dt)
  ode_system$integrate_const(stepper$ptr, ode_system$ptr, y, t0, t1, dt,
                             save_state)
}

##' @export
##' @rdname rodeint_integrate
integrate_n_steps <- function(stepper, ode_system, y, t0, dt, n,
                              save_state=FALSE) {
  assert_stepper(stepper)
  assert_ode_system(ode_system)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(dt)
  assert_scalar_size(n)
  ode_system$integrate_n_steps(stepper$ptr, ode_system$ptr, y, t0, dt, n,
                               save_state)
}

##' @export
##' @rdname rodeint_integrate
integrate_adaptive <- function(stepper, ode_system, y, t0, t1, dt,
                               save_state=FALSE) {
  if (inherits(stepper, "stepper_deSolve")) {
    return(integrate_adaptive_deSolve(stepper, ode_system, y, t0, t1, dt,
                                      save_state))
  }
  assert_stepper(stepper)
  assert_ode_system(ode_system)
  assert_numeric(y)
  assert_scalar_numeric(t0)
  assert_scalar_numeric(t1)
  assert_scalar_numeric(dt)
  ode_system$integrate_adaptive(stepper$ptr, ode_system$ptr, y, t0, t1, dt,
                                save_state)
}
##' @export
##' @rdname rodeint_integrate
integrate_times <- function(stepper, ode_system, y, times, dt) {
  assert_stepper(stepper)
  assert_ode_system(ode_system)
  assert_numeric(y)
  assert_numeric(times) # TODO: check sorted
  if (length(times) < 2) {
    stop("Must provide at least two times")
  }
  assert_scalar_numeric(dt)
  ode_system$integrate_times(stepper$ptr, ode_system$ptr, y, times, dt)
}

##' Helper function for binding ode_systems, steppers and integration
##' functions together.  This can be used to create a function
##' \code{f(y0, t)} from your system of \code{f\'(y0, t)}.
##'
##' Note the opposite ordering of the \code{ode_system} and \code{stepper}
##' arguments here compared with the rest of the package (following
##' \code{odeint}.
##'
##' @title Make Integration Function
##' @param ode_system A ode_system function (\code{\link{ode_system}}).
##' @param ... Additional arguments to bind.  Setting \code{t0} when a
##' system is time independent means \code{t1} will be a function of
##' elapsed time, which can be useful.  All integrate functions take a
##' \code{dt} argument, so that's useful to bind too.  You can also
##' pass in \code{set_as_defaults=TRUE} and the arguments, including
##' \code{stepper} and \code{ode_system} will simply be set as defaults
##' allowing some tuning later.
##' @param stepper A stepper object. By default the controlled
##' \code{runge_kutta_dopri} stepper is used with default tolerances.
##' If you want to change the tolerance, you must provide a different
##' stepper object.
##' @param integrate One of the integration functions.  The default is
##' \code{\link{integrate_adaptive}}.
##' @author Rich FitzJohn
##' @export
make_integrate <- function(ode_system, ..., stepper=NULL,
                           integrate=integrate_adaptive) {
  if (is.null(stepper)) {
    stepper <- make_stepper("controlled", "runge_kutta_dopri5")
  }
  assert_stepper(stepper)
  assert_ode_system(ode_system)
  ode_system <- ode_system$copy()
  ## TODO: Option to rewrite t1 -> t if t0 = 0?
  partially_apply(integrate, stepper=stepper, ode_system=ode_system, ...)
}

## Possible inefficiencies here -- the construction of the ode_system in
## the first place, the extra copy that happens during all
## make_integrate calls here.  I'm not concerned about it though - the
## copy should be fairly cheap and the startup cost should not be bad
## either.
##' @rdname make_integrate
##' @export
make_integrate_pars <- function(ode_system, ...) {
  ## First check that things work with the arguments we got (or we
  ## might not find out for ages).  This also deals with the issues
  ## described in ?force
  make_integrate(ode_system, ...)
  ode_system <- ode_system$copy()
  function(pars) {
    ode_system$set_pars(pars)
    make_integrate(ode_system, ...)
  }
}
