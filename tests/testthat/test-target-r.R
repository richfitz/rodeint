source("helper-rodeint.R")

context("target_r")

test_that("construction", {
  pars <- 0.5
  obj <- target_r$new(harmonic.oscillator, pars)
  expect_that(obj, is_a("target_r"))
  expect_that(obj$derivs.R <- harmonic.oscillator,
              throws_error("read-only"))
  expect_that(obj$ptr <- obj$ptr,
              throws_error("read-only"))
})

test_that("derivatives", {
  pars <- 0.5
  obj <- target_r$new(harmonic.oscillator, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic.oscillator(t0, y0, pars)))
})

test_that("parameters", {
  pars <- 0.5
  obj <- target_r$new(harmonic.oscillator, pars)
  expect_that(obj$pars(), is_identical_to(pars))
  ## Can hapilly set nonsense:
  pars2 <- list(a=1, b=2)
  obj$set_pars(pars2)
  expect_that(obj$pars(), is_identical_to(pars2))
})
