\name{integrate_n_steps}
\alias{integrate_n_steps}
\title{Adaptively Integrate an ODE System}
\usage{
integrate_n_steps(stepper, target, y, t0, dt, n, save_state = FALSE)
}
\arguments{
  \item{stepper}{A \code{stepper} object, created by
  \code{\link{stepper}} (other types will be supported
  soon).}

  \item{target}{The target system, created by
  \code{\link{target}}}

  \item{y}{Initial conditions}

  \item{t0}{Time to start the integration}

  \item{dt}{Step size}

  \item{n}{Number of steps}

  \item{save_state}{Return information about intermediate
  points as an attribute?}
}
\description{
Integrate a system of ODEs, taking a fixed number of fixed
size steps.
}
\author{
Rich FitzJohn
}

