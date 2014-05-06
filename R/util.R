assert_inherits <- function(x, what) {
  if (!inherits(x, what)) {
    stop(sprintf("%s must be a %s", deparse(substitute(x)),
                 paste(what, collapse=" / ")))
  }
}

assert_stepper <- function(x) {
  assert_inherits(x, "stepper")
}
