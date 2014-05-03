source("helper-rodeint.R")

context("target_r")

## This file is a gong show for now -- need to decide how to expose
## the wrapped up target functions: probably as little reference
## classes with methods (derivs, etc).  A similar problem exists for
## the stepper functions.

test_that("construction", {
  pars <- 0.5
  ode <- rodeint:::target_r__ctor(harmonic.oscillator, pars)
  expect_that(ode, is_a("externalptr"))
})

test_that("derivatives", {
  pars <- 0.5  
  ode <- rodeint:::target_r__ctor(harmonic.oscillator, pars)
  y0 <- c(0, 1)  
  expect_that(rodeint:::target_r__derivs(ode, y0, 0.0),
              equals(harmonic.oscillator(0.0, y0, pars)))
})
