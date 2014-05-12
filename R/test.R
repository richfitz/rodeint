test_harmonic_oscillator_r <- function(y, t, pars) {
  m.gam <- pars
  c(y[[2]], -y[[1]] - m.gam * y[[2]])
}

test_harmonic_oscillator_deSolve <- function(t, y, pars) {
  list(test_harmonic_oscillator_r(y, t, pars))
}

test_stiff_r_derivs <- function(y, t, pars) {
  c(-101.0 * y[1] - 100.0 * y[2],
    y[1])
}

## NOTE: Constant in t, y, pars -- should be exploitable
test_stiff_r_jacobian <- function(y, t, pars) {
  matrix(c(-101, 1, -100, 0), 2, 2)
}

test_stiff_r_derivs_deSolve <- function(t, y, pars) {
  list(test_stiff_r_derivs(y, t, pars))
}

test_stiff_r_jacobian_deSolve <- function(t, y, pars) {
  test_stiff_r_jacobian(y, t, pars)
}
