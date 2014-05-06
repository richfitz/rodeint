##' Controlled stepper (documentation coming)
##' @title Controlled Stepper
##' @aliases stepper_controlled
##' @rdname stepper_controlled
##' @export stepper_controlled
##' @export
stepper_controlled <- setRefClass("stepper_controlled",
                                  fields=list(
                                    category="character",
                                    type="character",
                                    atol="numeric",
                                    rtol="numeric",
                                    ptr="externalptr"))
stepper_controlled$lock(c("category", "type", "atol", "rtol", "ptr"))

stepper_controlled$methods(initialize=function(category, type, atol, rtol) {
  category <<- category
  type <<- type
  atol <<- atol
  rtol <<- rtol
  if (category == "basic") {
    ptr <<- rodeint:::stepper_basic__ctor(type)
  } else if (category == "controlled") {
    ptr <<- rodeint:::stepper_controlled__ctor(type, atol, rtol)
  } else {
    stop("Invalid category")
  }
})

## This is going to change at some point, but this is a list of the
## possible controlled stepper types.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{stepper_controlled}}.
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
  ## c("euler",
  ##   "modified_midpoint",
  ##   "runge_kutta4",
  ##   "runge_kutta_cash_karp54",
  ##   "runge_kutta_fehlberg78",
  ##   "runge_kutta_dopri5")
  "runge_kutta4"
}

##' @rdname stepper_controlled
##' @export
make_stepper_basic <- function(type) {
  stepper_controlled$new("basic", type, NA_real_, NA_real_)
}

##' @rdname stepper_controlled
##' @export
make_stepper_controlled <- function(type, atol=1e-6, rtol=1e-6) {
  stepper_controlled$new("controlled", type, atol, rtol)
}
