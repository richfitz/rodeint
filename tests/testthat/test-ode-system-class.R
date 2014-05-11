source("helper-rodeint.R")

context("ode_system (class)")

test_that("construction", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  expect_that(obj, is_a("ode_system"))
  expect_that(obj$type, is_identical_to("ode_system_class"))
  expect_that(obj$ptr <- obj$ptr,
              throws_error("read-only"))
})

test_that("derivatives", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic_oscillator_r(y0, t0, pars)))
})

test_that("parameters", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  expect_that(obj$get_pars(), is_identical_to(pars))
  pars2 <- list(a=1, b=2)
  ## In contrast with ode_system_r, this will throw:
  expect_that(obj$set_pars(pars2),
              throws_error("expecting a single value"))
  expect_that(obj$set_pars(rep(pars, 2)),
              throws_error("expecting a single value"))
  expect_that(obj$set_pars(numeric(0)),
              throws_error("expecting a single value"))
  expect_that(obj$get_pars(), is_identical_to(pars))
})

test_that("parameter validation", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars,
                    positive_scalar_numeric)
  ## Will throw on set:
  expect_that(obj$set_pars(-pars),         throws_error("Not positive"))
  expect_that(obj$set_pars(c(pars, pars)), throws_error("Not scalar"))
  expect_that(obj$set_pars(numeric(0)),    throws_error("Not scalar"))
  expect_that(obj$set_pars("pars"),        throws_error("Not numeric"))
  expect_that(obj$set_pars(list(pars)),    throws_error("Not numeric"))

  ## And on creation:
  expect_that(ode_system(harmonic_oscillator_r, -pars,
                         positive_scalar_numeric),
              throws_error("Not positive"))
  expect_that(ode_system(harmonic_oscillator_r, c(pars, pars),
                         positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(harmonic_oscillator_r, numeric(0),
                         positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(harmonic_oscillator_r, "pars",
                         positive_scalar_numeric),
              throws_error("Not numeric"))
  expect_that(ode_system(harmonic_oscillator_r, list(pars),
                         positive_scalar_numeric),
              throws_error("Not numeric"))
})

test_that("copying", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
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

test_that("deSolve interface", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  info <- obj$deSolve_info()
  expect_that(names(info),
              is_identical_to(c("func", "dllname", "initfunc", "initpar")))
  expect_that(info$func,     is_identical_to("deSolve_func_ode_system_class"))
  expect_that(info$dllname,  is_identical_to("rodeint"))
  expect_that(info$initfunc, is_identical_to("deSolve_initfunc"))
  expect_that(info$initpar,  is_a("externalptr"))
})

test_that("construction from deSolve type", {
  pars <- 0.5
  ## This would be the wrong generator anyway...
  expect_that(ode_system(harmonic_oscillator_cpp, pars,
                         deSolve_style=TRUE),
              throws_error("Not yet supported"))
})
