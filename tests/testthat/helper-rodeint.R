library(rodeint)
library(testthat)
library(deSolve) # for comparisons

## This is the same example as in odeint's harmonic_oscillator.cpp,
## with arguments in the same order as *deSolve*, not odeint.
harmonic.oscillator <- function(t, y, pars) {
  m.gam <- pars
  c(y[[2]], -y[[1]] - m.gam * y[[2]])
}

## Wrapper to turn functions like harmonic.oscillator into deSolve
## compatible ones.
wrap.deSolve <- function(f) function(...) list(f(...))

