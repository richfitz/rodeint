source("helper-rodeint.R")

context("ode_system (class)")

test_that("construction", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  expect_that(obj, is_a("ode_system"))
  expect_that(obj$type, is_identical_to("ode_system_class"))
})

test_that("show / print", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  expect_that(obj$show(), prints_text("ordinary differential equations"))
  expect_that(obj$show(), not(prints_text("Jacobian")))
  ## Detailed method:
  expect_that(obj$show(), not(prints_text("addr")))
  expect_that(obj$show(TRUE), prints_text("addr"))
  expect_that(obj$show(TRUE), prints_text("ode_system_class"))
})

test_that("derivatives", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic_oscillator_r(y0, t0, pars)))
})

test_that("jacobian", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$has_jacobian, is_false())
  expect_that(obj$jacobian(y0, t0),
              throws_error("System does not contain Jacobian"))
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

  pars2 <- pi
  obj$set_pars(pi)
  expect_that(obj$get_pars(), is_identical_to(pars2))
})

test_that("parameter validation", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars,
                    validate=positive_scalar_numeric)
  ## Will throw on set:
  expect_that(obj$set_pars(-pars),         throws_error("Not positive"))
  expect_that(obj$set_pars(c(pars, pars)), throws_error("Not scalar"))
  expect_that(obj$set_pars(numeric(0)),    throws_error("Not scalar"))
  expect_that(obj$set_pars("pars"),        throws_error("Not numeric"))
  expect_that(obj$set_pars(list(pars)),    throws_error("Not numeric"))

  ## And on creation:
  expect_that(ode_system(harmonic_oscillator_r, -pars,
                         validate=positive_scalar_numeric),
              throws_error("Not positive"))
  expect_that(ode_system(harmonic_oscillator_r, c(pars, pars),
                         validate=positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(harmonic_oscillator_r, numeric(0),
                         validate=positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(harmonic_oscillator_r, "pars",
                         validate=positive_scalar_numeric),
              throws_error("Not numeric"))
  expect_that(ode_system(harmonic_oscillator_r, list(pars),
                         validate=positive_scalar_numeric),
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

test_that("serialisation", {
  pars <- 0.5
  y0 <- c(0, 1)
  t0 <- 0.0
  obj <- ode_system(harmonic_oscillator_class, pars)
  f <- tempfile(fileext=".rds")
  saveRDS(obj, f)
  restored <- readRDS(f)
  expect_that(rodeint:::ptr_valid(restored$ptr), is_false())
  expect_that(restored$get_pars(), throws_error("NULL"))
  expect_that(restored$set_pars(pars), throws_error("NULL"))
  expect_that(restored$derivs(y0, t0), throws_error("NULL"))
  restored$rebuild()
  expect_that(rodeint:::ptr_valid(restored$ptr), is_true())
  expect_that(restored$get_pars(),
              is_identical_to(obj$get_pars()))
  expect_that(restored$derivs(y0, t0),
              is_identical_to(obj$derivs(y0, t0)))

  ## And again after setting parameters:
  pars2 <- pi
  obj$set_pars(pi)
  saveRDS(obj, f)
  restored <- readRDS(f)
  expect_that(restored$get_pars(), throws_error("NULL"))
  restored$rebuild()
  expect_that(restored$get_pars(),
              is_identical_to(obj$get_pars()))
})

test_that("deSolve interface", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_class, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  info <- obj$deSolve_info()
  expect_that(names(info),
              is_identical_to(c("func", "jacfunc",
                                "dllname", "initfunc", "initpar")))
  expect_that(info$func,     is_identical_to("deSolve_func_ode_system_class"))
  expect_that(info$jacfunc,  is_null())
  expect_that(info$dllname,  is_identical_to("rodeint"))
  expect_that(info$initfunc, is_identical_to("deSolve_initfunc"))
  expect_that(info$initpar,  is_a("externalptr"))
})

test_that("construction from deSolve type", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_deSolve_c, pars)
  expect_that(obj, is_a("ode_system"))
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic_oscillator_r(y0, t0, pars)))

  ## Parameters are set quite differently so quick check here:
  pars2 <- pi
  obj$set_pars(pars2)
  expect_that(obj$get_pars(), is_identical_to(pars2))
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic_oscillator_r(y0, t0, pars2)))

  expect_that(obj$set_pars(c(1, 2)),
              throws_error("Incorrect length"))
  expect_that(obj$set_pars(numeric(0)),
              throws_error("Incorrect length"))
  expect_that(obj$set_pars(NULL),
              throws_error("not compatible"))

  expect_that(obj$get_pars(), is_identical_to(pars2))
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic_oscillator_r(y0, t0, pars2)))
})

test_that("invalid construction from deSolve type", {
  pars <- 0.5
  ## This would be the wrong generator anyway...
  expect_that(ode_system(harmonic_oscillator_class, pars,
                         deSolve_style=TRUE),
              throws_error("Only meaningful for R functions"))
})
