\docType{class}
\name{stepper-class}
\alias{make_stepper}
\alias{make_stepper_basic}
\alias{make_stepper_controlled}
\alias{stepper}
\alias{stepper-class}
\title{Stepper}
\usage{
make_stepper_basic(type)

make_stepper_controlled(type, atol = 1e-06, rtol = 1e-06)

make_stepper(category, type, ...)
}
\arguments{
  \item{atol}{Absolute tolerance (see odeint docs for now)}

  \item{rtol}{Relative tolerance (see odeint docs for now)}

  \item{category}{Either "basic" or "controlled"}

  \item{type}{The type of stepper (e.g. "runge_kutta4")}

  \item{...}{Additional parameters passed from
  \code{make_stepper} to either \code{make_stepper_basic}
  (none allowed) or \code{make_stepper_controlled}.}
}
\description{
Stepper (documentation coming)
}

