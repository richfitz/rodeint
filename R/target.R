##' Integration targets (documentating coming)
##' @title Integration Target
##' @aliases target
##' @export target
##' @export
target <- setRefClass("target",
                        fields=list(
                          generator="function",
                          type="character",
                          ptr="externalptr",
                          # This section is a hack
                          .get_pars="function",
                          .set_pars="function",
                          .derivs="function",
                          # This is actually how it *should* be done.
                          integrate_const="function",
                          integrate_n_steps="function",
                          integrate_adaptive="function",
                          integrate_times="function",
                          integrate_simple="function"))

## Lock all fields:
target$lock(names(target$fields()))

target$methods(initialize = function(generator, pars, deSolve_style=FALSE) {
  obj <- generator_init(generator, pars, deSolve_style)
  generator <<- obj$generator
  type      <<- obj$type
  ptr       <<- obj$ptr

  ## NOTE: This is all so predictable that it could be done with
  ## runtime function lookup during initialise.
  if (type == "target_r") {
    .get_pars <<- target_r__get_pars
    .set_pars <<- target_r__set_pars
    .derivs   <<- target_r__derivs
    integrate_const    <<- r_integrate_const_r
    integrate_n_steps  <<- r_integrate_n_steps_r
    integrate_adaptive <<- r_integrate_adaptive_r
    integrate_times    <<- r_integrate_times_r
    integrate_simple   <<- r_integrate_simple_r
  } else if (type == "target_cpp") {
    .get_pars <<- target_cpp__get_pars
    .set_pars <<- target_cpp__set_pars
    .derivs   <<- target_cpp__derivs
    integrate_const    <<- r_integrate_const_cpp
    integrate_n_steps  <<- r_integrate_n_steps_cpp
    integrate_adaptive <<- r_integrate_adaptive_cpp
    integrate_times    <<- r_integrate_times_cpp
    integrate_simple   <<- r_integrate_simple_cpp
  } else if (type == "target_class") {
    .get_pars <<- target_class__get_pars
    .set_pars <<- target_class__set_pars
    .derivs   <<- target_class__derivs
    integrate_const    <<- r_integrate_const_class
    integrate_n_steps  <<- r_integrate_n_steps_class
    integrate_adaptive <<- r_integrate_adaptive_class
    integrate_times    <<- r_integrate_times_class
    integrate_simple   <<- r_integrate_simple_class
  } else {
    stop("Unsupported type (how did you even get here?)")
  }
})

## This is related to the hack above
target$methods(get_pars = function()     .get_pars(ptr))
target$methods(set_pars = function(pars) .set_pars(ptr, pars))
target$methods(derivs   = function(y, t) .derivs(ptr, y, t))

target$methods(copy=function() {
  target$new(generator, get_pars())
})

target$methods(deSolve_info = function() {
  if (type == "target_r") {
    target_pars <- get_pars()
    func <- function(t, y, ignored) { # we never allow extra args
      list(generator(y, t, target_pars))
    }
    dllname <- initfunc <- initpar <- NULL
  } else if (type %in% c("target_cpp", "target_class")) {
    func <- paste0("deSolve_func_", type)
    dllname <- "rodeint"
    initfunc <- "deSolve_initfunc"
    initpar <- ptr
  } else {
    stop("Unsupported type (how did you even get here?)")
  }
  list(func=func, dllname=dllname, initfunc=initfunc, initpar=initpar)
})

generator_init <- function(generator, pars, deSolve_style) {
  ## First step is to look at the provided function.  A single
  ## parameter function is assumed to be a generator.  A three
  ## parameter function is assumed to be a target function.
  if (length(formals(generator)) == 3) {
    if (deSolve_style) {
      derivs_deSolve <- generator
      generator <- function(y, t, pars) {
        derivs_deSolve(t, y, pars)[[1]]
      }
    }
    ptr <- target_r__ctor(generator, pars)
  } else if (length(formals(generator)) == 1) {
    if (deSolve_style) {
      stop("Not yet supported")
    }
    ptr <- generator(pars)
  }

  type=attr(ptr, "type")
  if (is.null(type)) {
    stop("Did not recieve a valid generator type")
  }

  list(type=type, generator=generator, ptr=ptr)
}
