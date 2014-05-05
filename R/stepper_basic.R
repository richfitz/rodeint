##' Basic stepper (documentation coming)
##' @title Basic Stepper
##' @aliases stepper_basic
##' @export stepper_basic
##' @export
stepper_basic <- setRefClass("stepper_basic",
                                  fields=list(
                                    name="character",
                                    ptr="externalptr"))
stepper_basic$lock(c("name", "ptr"))

stepper_basic$methods(initialize=function(name) {
  name <<- name
  ptr  <<- rodeint:::stepper_basic__ctor(name)
})

## This is going to change at some point, but this is a list of the
## possible basic stepper types.  Once I work out what to do
## about non-basic steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{stepper_basic}}.
##' @title Types of Basic Stepper
##' @author Rich FitzJohn
##' @export
stepper_basic_types <- function() {
  c("euler",
    "modified_midpoint",
    "runge_kutta4",
    "runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5")
}
