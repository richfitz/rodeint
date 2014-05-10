##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target_r
##' @export target_r
##' @export
target_r <- setRefClass("target_r",
                        fields=list(
                          derivs_R="function",
                          ptr="externalptr"))
target_r$lock(c("derivs_R", "ptr"))

target_r$methods(initialize = function(derivs, pars, deSolve_style=FALSE) {
  if (deSolve_style) {
    derivs_deSolve <- derivs
    derivs <- function(y, t, pars) {
      derivs_deSolve(t, y, pars)[[1]]
    }
  }
  derivs_R <<- derivs
  ptr <<- target_r__ctor(derivs_R, pars)
})

target_r$methods(pars = function() {
  target_r__get_pars(ptr)
})

target_r$methods(set_pars = function(pars) {
  target_r__set_pars(ptr, pars)
})

target_r$methods(derivs = function(y, t) {
  target_r__derivs(ptr, y, t)
})

## Depending on how useful they might be, and how sensible the
## reference class driven approach is, the following might be useful:
##   target_r$methods(make_integrate = function(...) {
##     make_integrate(.self, ...)
##   })
##   target_r$methods(make_integrate_pars = function(...) {
##     make_integrate_pars(.self, ...)
##   })
##   target_r$methods(integrate = function(...) {
##     make_integrate(...)()
##   })
## but I suspect that the generic approach might be more familiar, and
## more useful generally?

target_r$methods(copy=function() {
  target_r$new(derivs_R, pars())
})

target_r$methods(deSolve_func = function() {
  target_pars <- pars()
  function(t, y, ignored) { # we never allow extra args
    list(derivs_R(y, t, target_pars))
  }
})

target_r$methods(deSolve_info = function() {
  list(func=deSolve_func(), dllname=NULL,
       initfunc=NULL, initpar=NULL)
})

target_r$methods(odeint_integrate_const    = r_integrate_const_r)
target_r$methods(odeint_integrate_n_steps  = r_integrate_n_steps_r)
target_r$methods(odeint_integrate_adaptive = r_integrate_adaptive_r)
target_r$methods(odeint_integrate_times    = r_integrate_times_r)
target_r$methods(odeint_integrate_simple   = r_integrate_simple_r)

##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target_cpp
##' @export target_cpp
##' @export
target_cpp <- setRefClass("target_cpp",
                        fields=list(
                          generator="function",
                          ptr="externalptr"))
target_cpp$lock(c("generator", "ptr"))

target_cpp$methods(initialize = function(generator, pars) {
  generator <<- generator
  ptr <<- generator(pars)
})

target_cpp$methods(pars = function() {
  target_cpp__get_pars(ptr)
})

## For this, and for target_r, it would be nice to have a callback
## function that verifies the parameters.  A function that takes them
## as an argument and throws iff there's a problem would be OK.
target_cpp$methods(set_pars = function(pars) {
  target_cpp__set_pars(ptr, pars)
})

target_cpp$methods(derivs = function(y, t) {
  target_cpp__derivs(ptr, y, t)
})

target_cpp$methods(copy=function() {
  target_cpp$new(generator, pars())
})

target_cpp$methods(deSolve_info = function() {
  list(func="deSolve_func_target_cpp", dllname="rodeint",
       initfunc="deSolve_initfunc", initpar=ptr)
})

target_cpp$methods(odeint_integrate_const    = r_integrate_const_cpp)
target_cpp$methods(odeint_integrate_n_steps  = r_integrate_n_steps_cpp)
target_cpp$methods(odeint_integrate_adaptive = r_integrate_adaptive_cpp)
target_cpp$methods(odeint_integrate_times    = r_integrate_times_cpp)
target_cpp$methods(odeint_integrate_simple   = r_integrate_simple_cpp)

##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target_class
##' @export target_class
##' @export
target_class <- setRefClass("target_class",
                            fields=list(
                              generator="function",
                              ptr="externalptr"))
target_class$lock(c("generator", "ptr"))

target_class$methods(initialize = function(generator, pars) {
  generator <<- generator
  ptr <<- generator(pars)
})

target_class$methods(pars = function() {
  target_class__get_pars(ptr)
})

target_class$methods(set_pars = function(pars) {
  target_class__set_pars(ptr, pars)
})

target_class$methods(derivs = function(y, t) {
  target_class__derivs(ptr, y, t)
})

target_class$methods(copy=function() {
  target_class$new(generator, pars())
})

target_class$methods(deSolve_info = function() {
  list(func="deSolve_func_target_class", dllname="rodeint",
       initfunc="deSolve_initfunc", initpar=ptr)
})

target_class$methods(odeint_integrate_const    = r_integrate_const_class)
target_class$methods(odeint_integrate_n_steps  = r_integrate_n_steps_class)
target_class$methods(odeint_integrate_adaptive = r_integrate_adaptive_class)
target_class$methods(odeint_integrate_times    = r_integrate_times_class)
target_class$methods(odeint_integrate_simple   = r_integrate_simple_class)
