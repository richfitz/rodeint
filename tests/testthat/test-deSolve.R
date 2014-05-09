source("helper-rodeint.R")

context("deSolve interface")

test_that("deSolve", {
  pars <- 0.5
  ode_r <- target_r(harmonic.oscillator, pars)
  ode_cpp <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)
  ode_class <- target_class(rodeint:::test_harmonic_oscillator_class, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.05

  cmp <- lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator), pars)

  info_r <- ode_r$deSolve_info()
  info_cpp <- ode_cpp$deSolve_info()
  info_class <- ode_class$deSolve_info()

  fakepars <- NULL # totally ignored.
  res_r <- lsoda(y0, c(t0, t1),
                 parms=fakepars,
                 func=info_r$func,
                 dllname=info_r$dllname,
                 initfunc=info_r$initfunc,
                 initpar=info_r$initpar)
  res_cpp <- lsoda(y0, c(t0, t1),
                   pars=fakepars,
                   func=info_cpp$func,
                   dllname=info_cpp$dllname,
                   initfunc=info_cpp$initfunc,
                   initpar=info_cpp$initpar)
  res_class <- lsoda(y0, c(t0, t1),
                     pars=fakepars,
                     func=info_class$func,
                     dllname=info_class$dllname,
                     initfunc=info_class$initfunc,
                     initpar=info_class$initpar)

  ## These are going to be a bit at the mercy of deSolve's changes.
  expect_that(res_r, is_identical_to(cmp))
  attr(res_r, "lengthvar") <- NULL
  expect_that(res_cpp, equals(cmp, check.attributes=FALSE))
  expect_that(res_class, equals(cmp, check.attributes=FALSE))
  expect_that(res_cpp, is_identical_to(res_class))
})
