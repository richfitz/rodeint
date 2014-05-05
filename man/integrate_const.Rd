\name{integrate_const}
\alias{integrate_const}
\title{Adaptively Integrate an ODE System}
\usage{
integrate_const(stepper, target, y, t0, t1, dt, save_state = FALSE)
}
\arguments{
  \item{stepper}{A \code{stepper_controlled} object,
  created by \code{\link{stepper_controlled}} (other types
  will be supported soon).}

  \item{target}{The target system, created by
  \code{\link{target_r}}}

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

