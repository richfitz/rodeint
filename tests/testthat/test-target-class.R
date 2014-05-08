source("helper-rodeint.R")

context("target_class")

test_that("construction", {
  pars <- 0.5
  obj <- target_class(rodeint:::test_harmonic_oscillator_class, pars)
  expect_that(obj, is_a("target_class"))
  expect_that(obj$ptr <- obj$ptr,
              throws_error("read-only"))
})

test_that("derivatives", {
  pars <- 0.5
  obj <- target_class(rodeint:::test_harmonic_oscillator_class, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic.oscillator(t0, y0, pars)))
})

test_that("parameters", {
  pars <- 0.5
  obj <- target_class(rodeint:::test_harmonic_oscillator_class, pars)
  expect_that(obj$pars(), is_identical_to(pars))
  ## In contrast with target_r, this will throw:
  pars2 <- list(a=1, b=2)
  expect_that(obj$set_pars(pars2), throws_error("single value"))
  ## This should also throw but seems not to...
  expect_that(obj$set_pars(rep(pars, 2)), throws_error())
  expect_that(obj$set_pars(numeric(0)),   throws_error())
})
