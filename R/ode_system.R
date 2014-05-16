##' See the documenation for \code{initialize} for creating, and
##' generally methods below.  Examples and proper documentation
##' coming.
##' @title Systems of ODEs
##' @aliases ode_system
##' @export ode_system
##' @export
ode_system <- setRefClass("ode_system",
                          fields=list(
                            generator="function",
                            type="character",
                            ptr="externalptr",
                            validate="function",
                            # See explanation below
                            .get_pars="function",
                            .set_pars="function",
                            .derivs="function",
                            .jacobian="function",
                            has_jacobian="logical",
                            # These seem OK though
                            integrate_const="function",
                            integrate_n_steps="function",
                            integrate_adaptive="function",
                            integrate_times="function"))

## An explanation: I'd like to have the get_pars/set_pars/derivs
## methods dispatch fairly effiently to one of the three possible
## functions.  However, the branching only needs to be done at
## initialisation and after that things can't change (all fields are
## locked).  So the guts of the three functions is set into a field
## name beginning with a dot, which is initialised during
## initialize().  The actual method just passes the parameter to this
## function.

## Lock all fields:
ode_system$lock(names(ode_system$fields()))

## TODO: The argument name 'generator' is extremely confusing (here
## and in the stiff system section.  Perhaps wrap this with some
## helper function?
ode_system$methods(initialize = function(derivs, pars,
                     jacobian=NULL, validate=NULL,
                     deSolve_style=FALSE) {
  "\\itemize{
\\item \\code{derivs} Either
\\itemize{
\\item an R function of three parameters \\code{(y, t, pars)} that
returns a vector of derivatives (dy/dt), the same length as \\code{y}.
\\item as R function that, when run with the single argument
\\code{pars} returns a special object that will point at compiled
functions (\\code{ode_system_cpp}) or a compiled class
\\code{ode_system_class}. There will be more extensive documentation
coming on this soon.
}
\\item \\code{pars} the initial parameters, as accepted by your
function.  Generally this will be a numeric vector, but see
\\code{set_pars}.  This exists so that we can establish the correct
parameter vector length, and to ensure that all systems have
\\emph{something} initialised to prevent possible crashes or
unexpected behaviour.
\\item \\code{jacobian} If you are specifiying an R system and have
the jacobian, then this allows you to use stiff solvers
(e.g. \\code{rosenbrock4}).  It needs to be an R function of three
parameters \\code{(y, t, pars)} and return the Jacobian matrix.
\\item \\code{validate} optional function to run on parameters each
time they are set (see \\code{set_pars}).  Useful for checking length,
positivity, etc.  The function should throw an error (stop) if the
parameters are not valid.  The return value of this function is
ignored.
\\item \\code{deSolve_style} Set this to \\code{TRUE} if your R function
is in deSolve style rather than rodeint style (i.e., parameters
\\code{(t, y, pars)} and returning a \\emph{list} with the first
element being the derivatives).
}
"
  obj <- generator_init(derivs, pars, jacobian, deSolve_style)
  generator <<- obj$generator
  type      <<- obj$type
  ptr       <<- obj$ptr
  validate  <<- validate_init(validate)
  validate(pars)
  ## TODO: Tests.
  has_jacobian <<- attr(ptr, "has_jacobian")

  ## Becaue all the functions are named consistently, I'm using get()
  ## over a big if/else list.  It's nicer to look at, though is
  ## possibly a touch slower.
  get_rodeint <- function(...) {
    get(paste0(...), environment(ode_system), mode="function", inherits=FALSE)
  }

  .get_pars <<- get_rodeint(type, "__get_pars")
  .set_pars <<- get_rodeint(type, "__set_pars")
  .derivs   <<- get_rodeint(type, "__derivs")
  if (has_jacobian) {
    .jacobian <<- get_rodeint(type, "__jacobian")
  } else {
    .jacobian <<- function(...) {
      stop("System does not contain Jacobian")
    }
  }

  ty <- sub("ode_system_", "", type) # Abbrviated type :)
  integrate_const    <<- get_rodeint("integrate_const_",    ty)
  integrate_n_steps  <<- get_rodeint("integrate_n_steps_",  ty)
  integrate_adaptive <<- get_rodeint("integrate_adaptive_", ty)
  integrate_times    <<- get_rodeint("integrate_times_",    ty)
})

ode_system$methods(get_pars = function() {
  "Extract parameters from the object.  If you are using a class-based
ode_system, this is the parameters passed in to the object and is not
necessarily the same as the way you are choosing to store processed
parameters."
  .get_pars(ptr)
})
ode_system$methods(set_pars = function(pars) {
  "Set parameters within the object.

In addition to the validation carried out by \\code{validate},
different targets check in different ways (you only need to know about
this if you are writing targets):
\\itemize{
\\item \\code{ode_system_t}: No checking is done.  You can of course
check within the derivatives function.

\\item \\code{ode_system_cpp}: The parameters must be a numeric vector the
same length as the currently set parameters (so by extension the set
that it was initialised).  If you need variable length parameters you
will need to write a class. This might change once we have validation
functions.

\\item \\code{ode_system_class}: Parameters will be changed using whatever
you implemented in the \\code{set_pars} function, including what is
impled by the conversion using \\code{Rcpp::as<your_type>(SEXP)}.
}
"
  validate(pars)
  .set_pars(ptr, pars)
})

## TODO: Optionally pass a system size parameter to the initialisation
## funcion and sanitise system state here.
ode_system$methods(derivs = function(y, t) {
  "Compute the derivatives.  The first argument (\\code{y}) is the
system state and the second (\\code{t}) is the time.  No validation is
done on either variable.  This might change in the future."
  .derivs(ptr, y, t)
})

ode_system$methods(jacobian = function(y, t) {
  "Compute the Jacobian.  Not all systems can do this"
  .jacobian(ptr, y, t)
})

ode_system$methods(copy=function() {
  "Generate a copy of the object.  If you do not use this, then
the objects created by simple assignment will share parameters (i.e.,
setting parameters in the first object sets them in the second object
- or rather, they are the \\emph{same} object"
  ode_system$new(generator, get_pars(), validate=validate)
})

ode_system$methods(deSolve_info = function() {
  if (type == "ode_system_r") {
    p <- get_pars()
    func <- derivs_for_deSolve(environment(generator)$derivs, p)
    jacfunc <- dllname <- initfunc <- initpar <- NULL
  } else if (type == "ode_system_stiff_r") {
    p <- get_pars()
    func <- derivs_for_deSolve(environment(generator)$derivs, p)
    jacfunc <- jacobian_for_deSolve(environment(generator)$jacobian, p)
    dllname <- initfunc <- initpar <- NULL
  } else if (type %in% c("ode_system_cpp", "ode_system_class")) {
    func <- paste0("deSolve_func_", type)
    dllname <- "rodeint"
    initfunc <- "deSolve_initfunc"
    initpar <- ptr
    jacfunc <- NULL
  } else if (type %in% c("ode_system_stiff_cpp",
                         "ode_system_stiff_class")) {
    ## TODO: needs writing
    ## func     <- paste0("deSolve_func_", type)
    ## jacfunc  <- paste0("deSolve_jacobian_", type)
    ## dllname  <- "rodeint"
    ## initfunc <- "deSolve_initfunc"
    ## initpar <- ptr
    stop("Not yet supported")
  } else {
    stop("Unsupported type (how did you even get here?)")
  }
  list(func=func, jacfunc=jacfunc,
       dllname=dllname, initfunc=initfunc, initpar=initpar)
})

generator_init <- function(derivs, pars, jacobian, deSolve_style) {
  assert_function(derivs)

  if (length(formals(derivs)) == 3) { # R system.
    generator <- build_generator_r(derivs, jacobian, deSolve_style)
  } else {
    if (deSolve_style) {
      stop("Only meaningful for R functions")
    }
    if (!is.null(jacobian)) {
      ## TODO: Test
      stop("jacobian must be NULL except for R systems")
    }
    generator <- derivs
  }
  if (length(formals(generator)) != 1) {
    stop("Invalid generator")
  }

  ptr <- generator(pars)
  type <- attr(ptr, "type")
  if (!inherits(ptr, "externalptr") || is.null(type)) {
    stop("Generator did not return expected contents")
  }
  list(type=type, generator=generator, ptr=ptr)
}

build_generator_r <- function(derivs, jacobian, deSolve_style) {
  if (deSolve_style) {
    derivs <- derivs_from_deSolve(derivs)
  }

  if (is.null(jacobian)) {
    generator <- function(pars) {
      ode_system_r__ctor(derivs, pars)
    }
  } else {
    if (deSolve_style) {
      jacobian <- jacobian_from_deSolve(jacobian)
    }
    generator <- function(pars) {
      ode_system_stiff_r__ctor(derivs, jacobian, pars)
    }
  }
  generator
}

validate_init <- function(validate) {
  if (is.null(validate)) {
    function(pars) {}
  } else if (is.function(validate)) {
    validate
  } else {
    stop("validate must be a function")
  }
}

## Create functions *from* deSolve
derivs_from_deSolve <- function(derivs) {
  force(derivs)
  function(y, t, pars) {
    derivs(t, y, pars)[[1]]
  }
}
jacobian_from_deSolve <- function(jacobian) {
  force(jacobian)
  function(y, t, pars) {
    jacobian(t, y, pars)
  }
}

## Create for functions *for* deSolve
derivs_for_deSolve <- function(derivs, pars) {
  force(derivs)
  force(pars)
  function(t, y, ignored) { # we never allow extra args
    list(derivs(y, t, pars))
  }
}
jacobian_for_deSolve <- function(jacobian, pars) {
  force(jacobian)
  force(pars)
  function(t, y, ignored) { # we never allow extra args
    jacobian(y, t, pars)
  }
}
