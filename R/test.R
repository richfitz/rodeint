test_harmonic_oscillator_r <- function(y, t, pars) {
  m.gam <- pars
  c(y[[2]], -y[[1]] - m.gam * y[[2]])
}

test_harmonic_oscillator_deSolve <- function(t, y, pars) {
  list(test_harmonic_oscillator_r(y, t, pars))
}
