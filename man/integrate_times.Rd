\name{integrate_times}
\alias{integrate_times}
\title{Adaptively Integrate an ODE System}
\usage{
integrate_times(stepper, target, y, times, dt)
}
\arguments{
  \item{stepper}{A \code{controlled_stepper} object,
  created by \code{\link{controlled_stepper}} (other types
  will be supported soon).}

  \item{target}{The target system, created by
  \code{\link{target_r}}}

  \item{y}{Initial conditions}

  \item{times}{Vector of times.  First time is start time,
  last time is end time.}

  \item{dt}{Initial step size (will be tuned for controlled
  steppers -- see odeint documentation)}
}
\description{
Integrate a system of ODEs at fixed times (perhaps
adaptively). This version \emph{always} saves state -- that
is the reason for calling it.
}
\author{
Rich FitzJohn
}

