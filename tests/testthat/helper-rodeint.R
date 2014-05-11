library(rodeint)
library(testthat)
library(deSolve) # for comparisons

## From rodeint, for testing
harmonic_oscillator_r     <- rodeint:::test_harmonic_oscillator_r
harmonic_oscillator_cpp   <- rodeint:::test_harmonic_oscillator_cpp
harmonic_oscillator_class <- rodeint:::test_harmonic_oscillator_class
harmonic_oscillator_deSolve <- rodeint:::test_harmonic_oscillator_deSolve
harmonic_oscillator_deSolve_c <- rodeint:::test_harmonic_oscillator_deSolve_c

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
