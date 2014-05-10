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
                          # See explanation below
                          .get_pars="function",
                          .set_pars="function",
                          .derivs="function",
                          # These seem OK though
                          integrate_const="function",
                          integrate_n_steps="function",
                          integrate_adaptive="function",
                          integrate_times="function",
                          integrate_simple="function"))

## An explanation: I'd like to have the get_pars/set_pars/derivs
## methods dispatch fairly effiently to one of the three possible
## functions.  However, the branching only needs to be done at
## initialisation and after that things can't change (all fields are
## locked).  So the guts of the three functions is set into a field
## name beginning with a dot, which is initialised during
## initialize().  The actual method just passes the parameter to this
## function.

## Lock all fields:
target$lock(names(target$fields()))

target$methods(initialize = function(generator, pars, deSolve_style=FALSE) {
  obj <- generator_init(generator, pars, deSolve_style)
  generator <<- obj$generator
  type      <<- obj$type
  ptr       <<- obj$ptr

  ## Becaue all the functions are named consistently, I'm using get()
  ## over a big if/else list.  It's nicer to look at, though is
  ## possibly a touch slower.
  get_rodeint <- function(...) {
    get(paste0(...), environment(target), mode="function", inherits=FALSE)
  }

  .get_pars <<- get_rodeint(type, "__get_pars")
  .set_pars <<- get_rodeint(type, "__set_pars")
  .derivs   <<- get_rodeint(type, "__derivs")

  ty <- sub("target_", "", type) # Abbrviated type :)
  integrate_const    <<- get_rodeint("integrate_const_",    ty)
  integrate_n_steps  <<- get_rodeint("integrate_n_steps_",  ty)
  integrate_adaptive <<- get_rodeint("integrate_adaptive_", ty)
  integrate_times    <<- get_rodeint("integrate_times_",    ty)
  integrate_simple   <<- get_rodeint("integrate_simple_",   ty)
})

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
