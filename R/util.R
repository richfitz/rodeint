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
  assert_inherits(x, "stepper", name)
}

assert_target <- function(x, name=deparse(substitute(x))) {
  assert_inherits(x, c("target_r", "target_cpp", "target_class"), name)
}

assert_numeric <- function(x, name=deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name), call.=FALSE)
  }
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
