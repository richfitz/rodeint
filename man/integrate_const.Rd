\name{integrate_const}
\alias{integrate_const}
\title{Adaptively Integrate an ODE System}
\usage{
integrate_const(stepper, ode_system, y, t0, t1, dt, save_state = FALSE)
}
\arguments{
  \item{stepper}{A \code{stepper} object, created by
  \code{\link{stepper}} (other types will be supported
  soon).}

  \item{ode_system}{The ode_system system, created by
  \code{\link{ode_system}}}

  \item{y}{Initial conditions}

  \item{t0}{Time to start the integration}

  \item{t1}{Time to finish the integration}

  \item{dt}{Step size}

  \item{save_state}{Return information about intermediate
  points as an attribute?}
}
\description{
Integrate a system of ODEs, taking fixed steps
}
\author{
Rich FitzJohn
}

