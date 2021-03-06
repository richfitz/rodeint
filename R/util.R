## Why not use assert_that() here?  It's possibly a bit slow:
##   microbenchmark(assert_that(is.numeric(1)), assert_numeric(1))
## Lazy evaluation saves us most of the time, but most of the time in
## assert_that is spent on carefully evaluating things.  I'm open to
## moving to it.

assert_inherits <- function(x, what, name=deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("%s must be a %s", name,
                 paste(what, collapse=" / ")), call.=FALSE)
  }
}

assert_stepper <- function(x, name=deparse(substitute(x))) {
  assert_inherits(x, c("stepper", "stepper_stiff"), name)
}

assert_stepper_category <- function(category) {
  match_arg(category, stepper_categories(),
            "Invalid stepper category: %s must be one of %s")
}

assert_ode_system <- function(x, name=deparse(substitute(x))) {
  assert_inherits(x, c("ode_system", "ode_system_stiff"), name)
}

assert_function <- function(x, name=deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call.=FALSE)
  }
}

assert_nonnegative <- function(x, name=deparse(substitute(x))) {
  if (x < 0) {
    stop(sprintf("%s must be nonnegative", name), call.=FALSE)
  }
}

assert_numeric <- function(x, name=deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name), call.=FALSE)
  }
}

assert_integer <- function(x, strict=FALSE, name=deparse(substitute(x))) {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("%s must be integer", name), call.=FALSE)
    }
  }
}

## Useful for things handled with size_t, though these are passed
## through a function that will also warn.  This function is preferred
## though as it generates more useful error messages -- the compiled
## one prevents crashes!
assert_size <- function(x, strict=FALSE, name=deparse(substitute(x))) {
  assert_integer(x, strict, name)
  assert_nonnegative(x, name)
}

assert_logical <- function(x, name=deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("%s must be logical", name), call.=FALSE)
  }
}

assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}

assert_scalar_numeric <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}

assert_scalar_integer <- function(x, strict=FALSE,
                                  name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_integer(x, strict, name)
}

assert_scalar_logical <- function(x, strict=FALSE,
                                  name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_scalar_size <- function(x, strict=FALSE,
                               name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_size(x, strict, name)
}

assert_named <- function(x, empty_can_be_unnamed=TRUE,
                         name=deparse(substitute(x))) {
  if (is.null(names(x)) || any(names(x) == "")) {
    if (length(x) > 0 || !empty_can_be_unnamed) {
      stop(sprintf("%s must be named", name))
    }
  }
}

collapse <- function(x, sep=", ") {
  paste(x, collapse=sep)
}

## Like match.arg(), but does not allow for abbreviation.
match_arg <- function(arg, choices, msg=NULL) {
  assert_scalar(arg)
  if (!(arg %in% choices)) {
    if (is.null(msg)) {
      stop("'arg' must be one of ", collapse(dQuote(choices)))
    } else {
      stop(sprintf(msg, arg, collapse(dQuote(choices))))
    }
  }
}
