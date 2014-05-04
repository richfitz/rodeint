##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target_r
##' @export target_r
##' @export
target_r <- setRefClass("target_r",
                        fields=list(
                          "derivs.R"="function",
                          "ptr"="externalptr"))
target_r$lock(c("derivs.R", "ptr"))

target_r$methods(initialize = function(derivs, pars) {
  derivs.R <<- derivs
  ptr <<- rodeint:::target_r__ctor(derivs, pars)
})

target_r$methods(pars = function() {
  rodeint:::target_r__get_pars(ptr)
})

target_r$methods(set_pars = function(pars) {
  rodeint:::target_r__set_pars(ptr, pars)
})

target_r$methods(derivs = function(y, t) {
  rodeint:::target_r__derivs(ptr, y, t)
})
