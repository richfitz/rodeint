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
  target_r__get_pars(ptr)
})

target_r$methods(set_pars = function(pars) {
  target_r__set_pars(ptr, pars)
})

target_r$methods(derivs = function(y, t) {
  target_r__derivs(ptr, y, t)
})

## This would be cool, but better would also be to set a stepper.
## Could follow integrate_simple and just use rk_dopri5 by default?
##
## TODO: confirm that invalid input for stepper is caught without a
## crash.
target_r$methods(integrate_const =
                 function(stepper, y, t0, t1, dt, save_state=FALSE) {
  assert_stepper(stepper)
  r_integrate_const_r(stepper$ptr, ptr, y, t0, t1, dt, save_state)
})
target_r$methods(integrate_n_steps =
                 function(stepper, y, t0, dt, n, save_state=FALSE) {
  assert_stepper(stepper)
  r_integrate_n_steps_r(stepper$ptr, ptr, y, t0, dt, n, save_state)
})
target_r$methods(integrate_adaptive =
                 function(stepper, y, t0, t1, dt, save_state=FALSE) {
  assert_stepper(stepper)
  r_integrate_adaptive_r(stepper$ptr, ptr, y, t0, t1, dt, save_state)
})
target_r$methods(integrate_times =
                 function(stepper, y, times, dt) {
  assert_stepper(stepper)
  r_integrate_times_r(stepper$ptr, ptr, y, times, dt)
})
target_r$methods(integrate_simple =
                 function(y, t0, t1, dt, save_state=FALSE) {
  r_integrate_simple_r(ptr, y, t0, t1, dt, save_state)
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

target_cpp$methods(initialize = function(generator, pars) {
  generator <<- generator
  ptr <<- generator()
  ## TODO: Does this determine if the class is really a ptr?
  ## TODO: Can I use R class's set_pars(pars) directly?
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

target_cpp$methods(integrate_const =
                 function(stepper, y, t0, t1, dt, save_state=FALSE) {
  assert_stepper(stepper)
  r_integrate_const_cpp(stepper$ptr, ptr, y, t0, t1, dt, save_state)
})
target_cpp$methods(integrate_n_steps =
                 function(stepper, y, t0, dt, n, save_state=FALSE) {
  assert_stepper(stepper)
  r_integrate_n_steps_cpp(stepper$ptr, ptr, y, t0, dt, n, save_state)
})
target_cpp$methods(integrate_adaptive =
                 function(stepper, y, t0, t1, dt, save_state=FALSE) {
  assert_stepper(stepper)
  r_integrate_adaptive_cpp(stepper$ptr, ptr, y, t0, t1, dt, save_state)
})
target_cpp$methods(integrate_times =
                 function(stepper, y, times, dt) {
  assert_stepper(stepper)
  r_integrate_times_cpp(stepper$ptr, ptr, y, times, dt)
})
target_cpp$methods(integrate_simple =
                 function(y, t0, t1, dt, save_state=FALSE) {
  r_integrate_simple_cpp(ptr, y, t0, t1, dt, save_state)
})

##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target_class
##' @export target_class
##' @export
target_class <- setRefClass("target_class",
                            fields=list(
                              "generator"="function",
                              "ptr"="externalptr"))
target_class$lock(c("generator", "ptr"))

target_class$methods(initialize = function(generator, pars) {
  generator <<- generator
  ## TODO: Backport this style to the cpp version?
  ptr <<- generator(pars)
  ## TODO: Does this determine if the class is really a ptr?
})

target_class$methods(pars = function() {
  rodeint:::target_class__get_pars(ptr)
})

target_class$methods(set_pars = function(pars) {
  rodeint:::target_class__set_pars(ptr, pars)
})

target_class$methods(derivs = function(y, t) {
  rodeint:::target_class__derivs(ptr, y, t)
})
