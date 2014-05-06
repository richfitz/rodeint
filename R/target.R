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

##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target_cpp
##' @export target_cpp
##' @export
target_cpp <- setRefClass("target_cpp",
                        fields=list(
                          "generator"="function",
                          "ptr"="externalptr"))
target_cpp$lock(c("generator", "ptr"))

## TODO: This is going to change once we can look up function pointers
## by name.
target_cpp$methods(initialize = function(generator, pars) {
  generator <<- generator
  ptr <<- generator()
  ## TODO: Does this determine if the class is really a ptr?
  ## TODO: Can I use set_pars(pars) directly?
  rodeint:::target_cpp__set_pars(ptr, pars)
})

target_cpp$methods(pars = function() {
  rodeint:::target_cpp__get_pars(ptr)
})

## For this, and for target_r, it would be nice to have a callback
## function that verifies the parameters.  A function that takes them
## as an argument and throws iff there's a problem would be OK.
target_cpp$methods(set_pars = function(pars) {
  rodeint:::target_cpp__set_pars(ptr, pars)
})

target_cpp$methods(derivs = function(y, t) {
  rodeint:::target_cpp__derivs(ptr, y, t)
})
