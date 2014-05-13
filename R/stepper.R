##' Stepper (documentation coming)
##' @title Stepper
##' @aliases stepper
##' @rdname stepper
##' @export stepper
##' @export
stepper <- setRefClass("stepper",
                       fields=list(
                         category="character",
                         type="character",
                         atol="numeric",
                         rtol="numeric",
                         ptr="externalptr"))
stepper$lock(c("category", "type", "atol", "rtol", "ptr"))

stepper$methods(initialize=function(category, type, atol, rtol) {
  category <<- category
  type <<- type
  atol <<- atol
  rtol <<- rtol
  if (category == "basic") {
    ptr <<- stepper_basic__ctor(type)
  } else if (category == "controlled") {
    ptr <<- stepper_controlled__ctor(type, atol, rtol)
  } else {
    stop("Invalid stepper category")
  }
})

##' Stepper (documentation coming)
##' @title Stepper
##' @rdname stepper_stiff
##' @export stepper_stiff
##' @export
stepper_stiff <- setRefClass("stepper_stiff",
                             fields=list(
                         category="character",
                         type="character",
                         atol="numeric",
                         rtol="numeric",
                         ptr="externalptr"))

stepper_stiff$lock(names(stepper_stiff$fields()))

stepper_stiff$methods(initialize=function(category, atol=1e-6, rtol=1e-6) {
  category <<- category
  type <<- "rosenbrock4"
  if (category == "basic") {
    atol <<- NA_real_
    rtol <<- NA_real_
  } else {
    atol <<- atol
    rtol <<- rtol
  }

  if (type == "rosenbrock4") {
    ptr <<- stepper_stiff__ctor(category, atol, rtol)
  } else {
    stop("Invalid stepper type")
  }
})

## This is going to change at some point, but this is a list of the
## possible controlled stepper types.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{stepper}}.
##' @title Types of Controlled Stepper
##' @author Rich FitzJohn
##' @export
##' @rdname stepper_types
##' @param category Either "basic" or "controlled"
stepper_types <- function(category) {
  switch(category,
         basic=stepper_basic_types(),
         controlled=stepper_controlled_types(),
         stop("Invalid stepper category"))
}

##' @export
##' @rdname stepper_types
stepper_controlled_types <- function() {
  c("runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5")
}

##' @export
##' @rdname stepper_types
stepper_basic_types <- function() {
  c("euler",
    "modified_midpoint",
    "runge_kutta4",
    "runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5")
}

##' @export
##' @rdname stepper_types
stepper_categories <- function() {
  c("basic", "controlled")
}

## TODO: Once dense output support is there for the normal steppers
## this goes away.
##' @export
##' @rdname stepper_types
stepper_stiff_categories <- function() {
  c("basic", "controlled", "dense")
}

##' @rdname stepper
##' @export
make_stepper_basic <- function(type) {
  stepper("basic", type, NA_real_, NA_real_)
}

##' @rdname stepper
##' @export
##' @param atol Absolute tolerance (see odeint docs for now)
##' @param rtol Relative tolerance (see odeint docs for now)
make_stepper_controlled <- function(type, atol=1e-6, rtol=1e-6) {
  stepper("controlled", type, atol, rtol)
}

##' @rdname stepper
##' @export
##' @param category Either "basic" or "controlled"
##' @param type The type of stepper (e.g. "runge_kutta4")
##' @param ... Additional parameters passed from \code{make_stepper}
##' to either \code{make_stepper_basic} (none allowed) or
##' \code{make_stepper_controlled}.
make_stepper <- function(category, type, ...) {
  make <- switch(category,
                 basic=make_stepper_basic,
                 controlled=make_stepper_controlled,
                 stop("Invalid stepper category"))
  make(type, ...)
}
