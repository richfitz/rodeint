##' Integrate a system of ordinary differential equations (ODEs).
##' This is the low-level interface that corresponds exactly to the
##' \code{ideint} interface.  An easier to use, more R-ish, interface
##' is available using \code{\link{make_integrate}}.
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
##' \code{integrate_times}, which always returns information about
##' intermediate points.
##' @author Rich FitzJohn
##' @rdname rodeint_integrate
##' @seealso \code{\link{make_stepper}} for building steppers and for
##' information about possible algorithms, \code{\link{ode_system}} for
##' building a sytem of ODEs, and \code{\link{make_integrate}} for a
##' higher-level interface to these functions.
##'
##' The odeint documentation also has useful reference information on
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/odeint_in_detail/integrate_functions.html}{differences
##' between the four integrate functions}, and how
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/concepts/dense_output_stepper.html}{dense
##' output steppers} work.
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
