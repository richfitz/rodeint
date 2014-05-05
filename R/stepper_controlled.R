##' Controlled stepper (documentation coming)
##' @title Controlled Stepper
##' @aliases stepper_controlled
##' @export stepper_controlled
##' @export
stepper_controlled <- setRefClass("stepper_controlled",
                                  fields=list(
                                    name="character",
                                    atol="numeric",
                                    rtol="numeric",
                                    ptr="externalptr"))
stepper_controlled$lock(c("name", "atol", "rtol", "ptr"))

stepper_controlled$methods(initialize=function(name, atol=1e-6, rtol=1e-6) {
  name <<- name
  atol <<- atol
  rtol <<- rtol
  ptr  <<- rodeint:::stepper_controlled__ctor(name, atol, rtol)
})

## This is going to change at some point, but this is a list of the
## possible controlled stepper types.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{stepper_controlled}}.
##' @title Types of Controlled Stepper
##' @author Rich FitzJohn
##' @export
stepper_controlled_types <- function() {
  c("runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5")
}
