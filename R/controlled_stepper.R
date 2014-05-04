##' ##' Controlled stepper (documentating coming)
##' @title Controlled Stepper
##' @aliases controlled_stepper
##' @export controlled_stepper
##' @export
controlled_stepper <- setRefClass("controlled_stepper",
                                  fields=list(
                                    name="character",
                                    atol="numeric",
                                    rtol="numeric",
                                    ptr="externalptr"))
controlled_stepper$lock(c("name", "atol", "rtol", "ptr"))

controlled_stepper$methods(initialize=function(name, atol=1e-6, rtol=1e-6) {
  name <<- name
  atol <<- atol
  rtol <<- rtol
  ptr  <<- rodeint:::controlled_stepper__ctor(name, atol, rtol)
})

## This is going to change at some point, but this is a list of the
## possible controlled stepper types.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{controlled_stepper}}.
##' @title Types of Controlled Stepper
##' @author Rich FitzJohn
##' @export
controlled_stepper_types <- function() {
  c("runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5")
}
