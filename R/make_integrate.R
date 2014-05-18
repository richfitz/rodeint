##' Helper function for binding ode_systems, steppers and integration
##' functions together.  This can be used to create a function
##' \code{f(y0, t)} from your system of \code{f\'(y0, t)}.  These
##' functions reurn \emph{new functions} that have all the arguments
##' that won't change set.  For those interested, this form of
##' programming is nicely described
##' \href{http://adv-r.had.co.nz/Functional-programming.html#closures}{here},
##' (though you do not need to understand that to use these
##' functions).
##'
##' The motivation here is that often we just want to specify a set of
##' ODE solving parameters once and then solve a system many times -
##' at different initial conditions, or over different parameter
##' sets.  This function simplifies this approach by remembering
##' arguments passed in.
##'
##' Note the opposite ordering of the \code{ode_system} and \code{stepper}
##' arguments here compared with the rest of the package (following
##' \code{odeint}.  This is because a reasonable default stepper will
##' be chosen if none is provided, but a system of ODEs is always
##' required!
##'
##' The function \code{make_integrate_pars} is a higher-higher order
##' function.  It returns a function that takes parameters of the
##' system as an argument.  When \emph{that} function is run it
##' returns a function with arguments bound as for
##' \code{make_integrate}.  This is a lot simpler than it sounds - see
##' the final example.
##'
##' @title Make Integration Function
##' @param ode_system A ode_system function (\code{\link{ode_system}}).
##' @param ... Additional \emph{named} arguments to bind.  Setting
##' \code{t0} when a system is time independent means \code{t1} will
##' be a function of elapsed time, which can be useful.  All integrate
##' functions take a \code{dt} argument, so that's useful to bind too.
##' You can also pass in \code{set_as_defaults=TRUE} and the
##' arguments, including \code{stepper} and \code{ode_system} will
##' simply be set as defaults allowing some tuning later.
##' @param stepper A stepper object. By default the controlled
##' \code{runge_kutta_dopri} stepper is used with default tolerances.
##' If you want to change the tolerance, you must provide a different
##' stepper object.
##' @param integrate One of the integration functions.  The default is
##' \code{\link{integrate_adaptive}}.
##' @author Rich FitzJohn
##'
##' @seealso \code{\link{integrate_adaptive}}, which this function
##' wraps around, and \code{\link{ode_system}} for building a system
##' of ODEs to integrate.
##'
##' @examples
##'
## Picking up on the example in ?integrate_adaptive:
##'
##' ## A system of differential equations describing a harmonic oscillator:
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
##' ## To integrate this system, we need to specify a stepper, initial
##' ## conditions, begining and end times and an initial step size:
##' s <- make_stepper("dense", "runge_kutta_dopri5")
##' y0 <- c(0, 1)
##' t0 <- 0
##' t1 <- 5
##' dt <- 0.01
##' integrate_adaptive(s, sys, y0, t0, t1, dt)
##'
##' ## If we wanted to run this from a couple of different starting points
##' ## it gets quite tedious:
##' y1 <- c(1, 1)
##' y2 <- c(2, 1)
##' y3 <- c(3, 1)
##' integrate_adaptive(s, sys, y0, t0, t1, dt)
##' integrate_adaptive(s, sys, y1, t0, t1, dt)
##' integrate_adaptive(s, sys, y2, t0, t1, dt)
##' integrate_adaptive(s, sys, y3, t0, t1, dt)
##'
##' ## There is a lot of repetition here, and it's not actually that clear
##' ## what is changing.  The make_integrate functions binds arguments
##' ## that won't change (all arguments must be named!)
##' f <- make_integrate(sys, t0=t0, t1=t1, dt=dt, stepper=s)
##'
##' ## Our function now has only a single required argument (y) and an
##' ## optional argument "save_state":
##' args(f)
##'
##' ## This is because all the arguments in integrate_adaptive that
##' ## matched the list given were set to the values provided.
##'
##' ## This new function can then be run like so:
##' f(y0)
##' f(y1)
##' f(y2)
##' f(y3)
##'
##' ## Or, to get intermediate values out:
##' f(y0, save_state=TRUE)
##'
##' ## If we also wanted 't1' to be left free, just don't provide it to
##' ## make_integrate:
##' f <- make_integrate(sys, t0=t0, dt=dt, stepper=s)
##' args(f)
##'
##' ## We now have a function f(y, t1).  Note that this approach has
##' ## convered a function dy/dt (derivs) into a function simply of time
##' ## and initial conditions.
##'
##' ## This approach works with integrate_times, too, by specifying that
##' ## as the "integrate" argument:
##' f_at_times <- make_integrate(sys, dt=dt, stepper=s,
##'                              integrate=integrate_times)
##'
##' ## This returns a function taking "y" and "times" as arguments (no
##' ## save_state, because integrate_times does not accept that argument.
##' args(f_at_times)
##'
##' ## It can be run like so:
##' tt <- seq(0, 5, length=21)
##' f_at_times(y0, tt)
##'
##' ## Finally, sometimes it is useful to easily apply the function over a
##' ## series of parameters.  To do this, use make_integrate_pars.  The
##' ## calling sequence is the same as above.
##' g <- make_integrate_pars(sys, t0=t0, dt=dt, stepper=s)
##'
##' ## This returns a function of 'pars' only:
##' args(g)
##'
##' ## When run with parameters, this function returns a function of y and t1:
##' args(g(pars))
##'
##' ## So
##' g(pars)(y0, t1)
##'
##' ## sets parametes to pars, and then integrates with initial conditions
##' ## y0 up until time t1, using t0, dt and the stepper provided above.
##'
##' ## This makes it easy to explore how the system changes over parameter
##' ## space
##' g <- make_integrate_pars(sys, y=y0, t0=t0, t1=1, dt=dt, stepper=s)
##'
##' g(pars * 0.9)()
##' g(pars)()
##' g(pars * 1.1)()
##'
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

## TODO: Consider an option (as_generator) which toggles between this
## behaviour and one where pars is set as an argument (first
## argument).  Requires some trickery with passing everything in or we
## get an unhelpful function of dot arguments.

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
