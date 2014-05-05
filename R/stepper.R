##' Stepper (documentation coming)
##' @title Stepper
##' @aliases stepper
##' @export stepper
##' @export
stepper <- setRefClass("stepper",
                             fields=list(
                               type="character",
                               subtype="character",
                               ptr="externalptr"))
stepper$lock(c("type", "subtype", "ptr"))

stepper$methods(initialize=function(type, subtype, atol=1e-6, rtol=1e-6) {
  type    <<- type
  subtype <<- subtype
  ptr     <<- rodeint:::stepper__ctor(type, subtype, atol, rtol)
})

##' Valid values for making a \code{\link{stepper}}.
##' @title Types of Stepper
##' @author Rich FitzJohn
##' @export
stepper_types <- function() {
  c("basic", "controlled")
}
