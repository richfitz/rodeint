\name{make_integrate}
\alias{make_integrate}
\alias{make_integrate_pars}
\title{Make Integration Function}
\usage{
make_integrate(ode_system, ..., stepper = NULL,
  integrate = integrate_adaptive)

make_integrate_pars(ode_system, ...)
}
\arguments{
  \item{ode_system}{A ode_system function
  (\code{\link{ode_system}}).}

  \item{...}{Additional arguments to bind.  Setting
  \code{t0} when a system is time independent means
  \code{t1} will be a function of elapsed time, which can
  be useful.  All integrate functions take a \code{dt}
  argument, so that's useful to bind too.  You can also
  pass in \code{set_as_defaults=TRUE} and the arguments,
  including \code{stepper} and \code{ode_system} will
  simply be set as defaults allowing some tuning later.}

  \item{stepper}{A stepper object. By default the
  controlled \code{runge_kutta_dopri} stepper is used with
  default tolerances. If you want to change the tolerance,
  you must provide a different stepper object.}

  \item{integrate}{One of the integration functions.  The
  default is \code{\link{integrate_adaptive}}.}
}
\description{
Helper function for binding ode_systems, steppers and
integration functions together.  This can be used to create
a function \code{f(y0, t)} from your system of
\code{f\'(y0, t)}.
}
\details{
Note the opposite ordering of the \code{ode_system} and
\code{stepper} arguments here compared with the rest of the
package (following \code{odeint}.
}
\author{
Rich FitzJohn
}

