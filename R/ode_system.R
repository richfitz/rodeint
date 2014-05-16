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
ode_system$lock(names(ode_system$fields()))

## TODO: The argument name 'generator' is extremely confusing (here
## and in the stiff system section.  Perhaps wrap this with some
## helper function?
ode_system$methods(initialize = function(generator, pars,
                     validate=NULL, deSolve_style=FALSE) {
  "\\itemize{
\\item \\code{generator} Either
\\itemize{
\\item an R function of three parameters \\code{(y, t, pars)} that
returns a vector of derivatives (dy/dt), the same length as \\code{y}.
\\item as R function that, when run with the single argument
\\code{pars} returns a special object that will point at compiled
functions (\\code{ode_system_cpp}) or a compiled class
\\code{ode_system_class}.  There will be more extensive documentation
coming on this soon.
}
\\item \\code{pars} the initial parameters, as accepted by your
function.  Generally this will be a numeric vector, but see
\\code{set_pars}.
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
  obj <- generator_init(generator, pars, deSolve_style)
  generator <<- obj$generator
  type      <<- obj$type
  ptr       <<- obj$ptr
  validate  <<- validate_init(validate)
  validate(pars)

  .get_pars <<- get_rodeint(type, "__get_pars")
  .set_pars <<- get_rodeint(type, "__set_pars")
  .derivs   <<- get_rodeint(type, "__derivs")

  ty <- sub("ode_system_", "", type) # Abbrviated type :)
  integrate_const    <<- get_rodeint("integrate_const_",    ty)
  integrate_n_steps  <<- get_rodeint("integrate_n_steps_",  ty)
  integrate_adaptive <<- get_rodeint("integrate_adaptive_", ty)
  integrate_times    <<- get_rodeint("integrate_times_",    ty)
  integrate_simple   <<- get_rodeint("integrate_simple_",   ty)
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

ode_system$methods(copy=function() {
  "Generate a copy of the object.  If you do not use this, then
the objects created by simple assignment will share parameters (i.e.,
setting parameters in the first object sets them in the second object
- or rather, they are the \\emph{same} object"
  ode_system$new(generator, get_pars(), validate)
})

ode_system$methods(deSolve_info = function() {
  if (type == "ode_system_r") {
    ode_system_pars <- get_pars()
    func <- function(t, y, ignored) { # we never allow extra args
      list(generator(y, t, ode_system_pars))
    }
    dllname <- initfunc <- initpar <- NULL
  } else if (type %in% c("ode_system_cpp", "ode_system_class")) {
    func <- paste0("deSolve_func_", type)
    dllname <- "rodeint"
    initfunc <- "deSolve_initfunc"
    initpar <- ptr
  } else {
    stop("Unsupported type (how did you even get here?)")
  }
  jacfunc <- NULL
  list(func=func, jacfunc=jacfunc,
       dllname=dllname, initfunc=initfunc, initpar=initpar)
})

generator_init <- function(generator, pars, deSolve_style) {
  assert_function(generator)

  ## First step is to look at the provided function.  A single
  ## parameter function is assumed to be a generator.  A three
  ## parameter function is assumed to be a ode_system function.
  if (length(formals(generator)) == 3) {
    if (deSolve_style) {
      derivs_deSolve <- generator
      generator <- function(y, t, pars) {
        derivs_deSolve(t, y, pars)[[1]]
      }
    }
    ptr <- ode_system_r__ctor(generator, pars)
  } else if (deSolve_style) {
    stop("Only meaningful for R functions")
  } else if (length(formals(generator)) == 1) {
    if (deSolve_style) {
      stop("Only meaningful for R functions")
    }
    ptr <- generator(pars)
  } else { ## TODO: test this
    stop("Invalid generator")
  }

  type=attr(ptr, "type")
  if (is.null(type)) {
    stop("Did not recieve a valid generator type")
  }

  list(type=type, generator=generator, ptr=ptr)
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

##' Temporary only - will merge into ode_system
##' @title Stiff Systems of ODEs
##' @aliases ode_system_stiff
##' @export ode_system_stiff
##' @export
ode_system_stiff <- setRefClass("ode_system_stiff",
                                fields=list(
                                  ## Might still change
                                  generator="function",
                                  type="character",
                                  ptr="externalptr",
                                  validate="function",
                                  ## See explanation above
                                  .get_pars="function",
                                  .set_pars="function",
                                  .derivs="function",
                                  .jacobian="function", # +dfdt soon?
                                  # These seem OK though
                                  integrate_const="function",
                                  integrate_n_steps="function",
                                  integrate_adaptive="function",
                                  integrate_times="function"))
## This one needs a good error message or something.
# integrate_simple="function"))

ode_system_stiff$lock(names(ode_system_stiff$fields()))

ode_system_stiff$methods(initialize = function(generator, pars,
                           validate=NULL, deSolve_style=FALSE) {
  obj <- generator_init_stiff(generator, pars, deSolve_style)
  generator <<- obj$generator
  type      <<- obj$type
  ptr       <<- obj$ptr
  validate  <<- validate_init(validate)
  validate(pars)

  .get_pars <<- get_rodeint(type, "__get_pars")
  .set_pars <<- get_rodeint(type, "__set_pars")
  .derivs   <<- get_rodeint(type, "__derivs")
  .jacobian <<- get_rodeint(type, "__jacobian")

  ty <- sub("ode_system_", "", type) # Abbrviated type :)
  integrate_const    <<- get_rodeint("integrate_const_",    ty)
  integrate_n_steps  <<- get_rodeint("integrate_n_steps_",  ty)
  integrate_adaptive <<- get_rodeint("integrate_adaptive_", ty)
  integrate_times    <<- get_rodeint("integrate_times_",    ty)
  ## integrate_simple   <<- get_rodeint("integrate_simple_",   ty)
})

ode_system_stiff$methods(get_pars = function() {
  .get_pars(ptr)
})
ode_system_stiff$methods(set_pars = function(pars) {
  validate(pars)
  .set_pars(ptr, pars)
})

ode_system_stiff$methods(derivs = function(y, t) {
  .derivs(ptr, y, t)
})
ode_system_stiff$methods(jacobian = function(y, t) {
  .jacobian(ptr, y, t)
})

ode_system_stiff$methods(copy=function() {
  ode_system_stiff$new(generator, get_pars(), validate)
})

ode_system_stiff$methods(deSolve_info = function() {
  if (type == "ode_system_stiff_r") {
    ode_system_pars <- get_pars()
    contents <- generator()
    derivs <- contents$derivs
    jacobian <- contents$jacobian
    func <- function(t, y, ignored) { # we never allow extra args
      list(derivs(y, t, ode_system_pars))
    }
    jacfunc <- function(t, y, ignored) {
      jacobian(y, t, ode_system_pars)
    }
  } else if (type %in% c("ode_system_stiff_cpp",
                         "ode_system_stiff_class")) {
    ## TODO: needs writing
    ## func     <- paste0("deSolve_func_", type)
    ## jacfunc  <- paste0("deSolve_func_", type)
    ## dllname  <- "rodeint"
    ## initfunc <- "deSolve_initfunc"
    ## initpar <- ptr
    stop("Not yet supported")
  } else {
    stop("Unsupported type (how did you even get here?)")
  }
  dllname <- initfunc <- initpar <- NULL
  list(func=func, jacfunc=jacfunc,
       dllname=dllname, initfunc=initfunc, initpar=initpar)
})

generator_init_stiff <- function(generator, pars, deSolve_style) {
  ## This is one that we've built already
  if (is.function(generator) && length(formals(generator)) == 0) {
    generator <- generator()
  }

  if (is.list(generator)) { # ode_system_r
    if (!identical(names(generator), c("derivs", "jacobian"))) {
      stop("Invalid generator contents")
    }
    if (deSolve_style) {
      contents <- stiff_system(generator$derivs, generator$jacobian,
                               deSolve_style)
    } else {
      contents <- generator
    }
    ## This little dance is a workaround getting the "generator" slot
    ## typed correctly.  Should make an S3 class that could be
    ## either.
    generator <- function() contents
    ptr <- ode_system_stiff_r__ctor(contents$derivs, contents$jacobian, pars)
  } else if (deSolve_style) {
    stop("Only meaningful for R functions")
  } else if (is.function(generator) && length(formals(generator)) == 1) {
    ptr <- generator(pars)
  } else {
    stop("Invalid generator contents")
  }

  type <- attr(ptr, "type")
  if (is.null(type)) {
    stop("Did not recieve a valid generator type")
  }
  list(type=type, generator=generator, ptr=ptr)
}

## Becaue all the functions are named consistently, I'm using get()
## over a big if/else list.  It's nicer to look at, though is
## possibly a touch slower.
get_rodeint <- function(...) {
  get(paste0(...), environment(ode_system), mode="function", inherits=FALSE)
}

## Odd little function, has a job.
##
## The logic around handling these things is really hard, and will
## take some getting right.  I can probably simplify it once I know
## exactly what the interface will be doing though.  This is going to
## be the
stiff_system <- function(derivs, jacobian, deSolve_style=FALSE) {
  assert_function(derivs)
  assert_function(jacobian)
  ## TODO: check parameters here for t first as a deSolve style, and
  ## allow it to be overridden with an argument.  Seems a bit cute and
  ## error prone.  Put it up as a RFC on github and stick in a check
  ## warning.
  ##
  ## NOTE: can't ever specify dfdt for deSolve models - assume
  ## constant once we support it?
  if (deSolve_style) {
    list(derivs  =function(y, t, pars) derivs(t, y, pars)[[1]],
         jacobian=function(y, t, pars) jacobian(t, y, pars))
  } else {
    list(derivs=derivs, jacobian=jacobian)
  }
}
