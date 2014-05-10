source("helper-rodeint.R")

context("target (r)")

test_that("construction", {
  pars <- 0.5
  obj <- target(harmonic.oscillator, pars)
  expect_that(obj, is_a("target"))
  expect_that(obj$type, is_identical_to("target_r"))
  expect_that(obj$ptr <- obj$ptr,
              throws_error("read-only"))
})

test_that("derivatives", {
  pars <- 0.5
  obj <- target(harmonic.oscillator, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic.oscillator(y0, t0, pars)))
})

## Pending....
## test_that("construction from deSolve type", {
##   pars <- 0.5
##   obj <- target(harmonic.oscillator.deSolve, pars, TRUE)
##   expect_that(obj, is_a("target"))
##   y0 <- c(0, 1)
##   t0 <- 0.0
##   expect_that(obj$derivs(y0, t0),
##               is_identical_to(harmonic.oscillator(y0, t0, pars)))
##   expect_that(obj$deSolve_func()(t0, y0, ignored),
##               is_identical_to(harmonic.oscillator.deSolve(t0, y0, pars)))
## })

test_that("parameters", {
  pars <- 0.5
  obj <- target(harmonic.oscillator, pars)
  expect_that(obj$get_pars(), is_identical_to(pars))
  ## Can hapilly set nonsense:
  pars2 <- list(a=1, b=2)
  obj$set_pars(pars2)
  expect_that(obj$get_pars(), is_identical_to(pars2))
})

test_that("copying", {
  pars <- 0.5
  obj <- target(harmonic.oscillator, pars)
  expect_that(obj$get_pars(), is_identical_to(pars))

  obj.same <- obj        # not a copy
  obj.copy <- obj$copy() # is a copy

  expect_that(obj.same$ptr, is_same_pointer(obj$ptr))
  expect_that(obj.copy$ptr, not(is_same_pointer(obj$ptr)))

  pars2 <- pi
  obj$set_pars(pars2)
  expect_that(obj.same$get_pars(), is_identical_to(pars2))
  expect_that(obj.copy$get_pars(), not(is_identical_to(pars2)))

  pars3 <- exp(1)
  obj.copy$set_pars(pars3)
  expect_that(obj.same$get_pars(), is_identical_to(pars2))
  expect_that(obj.copy$get_pars(), is_identical_to(pars3))
})

## Pending
test_that("deSolve interface", {
  pars <- 0.5
  obj <- target(harmonic.oscillator, pars)
  y0 <- c(0, 1)
  t0 <- 0.0

  info <- obj$deSolve_info()
  expect_that(names(info),
              is_identical_to(c("func", "dllname", "initfunc", "initpar")))
  expect_that(info$func,     is_a("function"))
  expect_that(info$dllname,  is_identical_to(NULL))
  expect_that(info$initfunc, is_identical_to(NULL))
  expect_that(info$initpar,  is_identical_to(NULL))

  expect_that(info$func(t0, y0, pars),
              is_identical_to(harmonic.oscillator.deSolve(t0, y0, pars)))

})
