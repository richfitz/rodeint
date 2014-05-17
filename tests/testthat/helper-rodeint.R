library(rodeint)
library(testthat)
library(deSolve) # for comparisons

## From rodeint, for testing
harmonic_oscillator_r     <- rodeint:::test_harmonic_oscillator_r
harmonic_oscillator_cpp   <- rodeint:::test_harmonic_oscillator_cpp
harmonic_oscillator_class <- rodeint:::test_harmonic_oscillator_class
harmonic_oscillator_deSolve <- rodeint:::test_harmonic_oscillator_deSolve
harmonic_oscillator_deSolve_c <- rodeint:::test_harmonic_oscillator_deSolve_c

## TODO: Helper function for validating R derivative/jacobian pairs,
## possibly also for associating a dfdt type or specifying a default.
stiff_r_derivs <- rodeint:::test_stiff_r_derivs
stiff_r_jacobian <- rodeint:::test_stiff_r_jacobian
stiff_cpp <- rodeint:::test_stiff_cpp
stiff_class <- rodeint:::test_stiff_class

stiff_r_derivs_deSolve <- rodeint:::test_stiff_r_derivs_deSolve
stiff_r_jacobian_deSolve <- rodeint:::test_stiff_r_jacobian_deSolve

last <- function(x) {
  x[[length(x)]]
}
last_row <- function(m) {
  m[nrow(m),]
}

is_in_range <- function(a, b) {
  collapse <- function(x) {
    if (length(x) == 1) paste(x) else
    sprintf("{%s}", paste(x, collapse=", "))
  }
  function(x) {
    ok <- sign(x - a) == sign(b - x)
    expectation(all(ok), sprintf("is not in range [%s, %s]",
                                 collapse(a), collapse(b)))
  }
}

is_same_pointer <- function(ptr) {
  name <- deparse(substitute(ptr))
  function(x) {
    expectation(identical(x, ptr),
                paste("is not the same pointer as", name),
                paste("is the same pointer as", name))
  }
}

positive_scalar_numeric <- function(x) {
  if (!is.numeric(x)) {
    stop("Not numeric")
  } else if (length(x) != 1) {
    stop("Not scalar")
  } else if (x < 0) {
    stop("Not positive")
  }
}

can_make_stepper <- function(category, algorithm, have_jacobian=FALSE) {
  algorithm %in% stepper_algorithms(category, have_jacobian)
}
