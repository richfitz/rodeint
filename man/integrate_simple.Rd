\name{integrate_simple}
\alias{integrate_simple}
\title{Integrate an ODE System}
\usage{
integrate_simple(ode_system, y, t0, t1, dt, save_state = FALSE)
}
\arguments{
  \item{ode_system}{The ode_system system, created by
  \code{\link{ode_system}}}

  \item{y}{Initial conditions}

  \item{t0}{Time to start the integration}

  \item{t1}{Time to finish the integration (not sure if we
  hit this one exactly)}

  \item{dt}{Step size}

  \item{save_state}{Return information about intermediate
  points as}
}
\description{
Integrate a system of ordinary differential equations.
This is just a convenience function (in odeint) and
probably should not be the final function used.
}
\author{
Rich FitzJohn
}

