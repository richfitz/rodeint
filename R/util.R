assert_inherits <- function(x, what) {
  if (!inherits(x, what)) {
    stop(sprintf("%s must be a %s", deparse(substitute(x)),
                 paste(what, collapse=" / ")))
  }
}

assert_target_r <- function(x) {
  assert_inherits(x, "target_r")
}

assert_stepper_controlled <- function(x) {
  assert_inherits(x, "stepper_controlled")
}

assert_stepper_basic <- function(x) {
  assert_inherits(x, "stepper_basic")
}
