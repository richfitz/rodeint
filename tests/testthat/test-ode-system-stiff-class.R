source("helper-rodeint.R")

context("ode_system (with jacobian, class)")

test_that("construction", {
  pars <- numeric(0)
  obj <- ode_system(stiff_class, pars)
  expect_that(obj, is_a("ode_system"))
  expect_that(obj$type, is_identical_to("ode_system_stiff_class"))
})

test_that("show / print", {
  pars <- numeric(0)
  obj <- ode_system(stiff_class, pars)
  expect_that(obj$show(), prints_text("ordinary differential equations"))
  expect_that(obj$show(), prints_text("Jacobian"))
  ## Detailed method:
  expect_that(obj$show(), not(prints_text("addr")))
  expect_that(obj$show(TRUE), prints_text("addr"))
  expect_that(obj$show(TRUE), prints_text("ode_system_stiff_class"))
})

test_that("derivatives", {
  pars <- numeric(0)
  obj <- ode_system(stiff_class, pars)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  expect_that(obj$derivs(y0, t0),
              is_identical_to(stiff_r_derivs(y0, t0, pars)))
})

test_that("jacobian", {
  pars <- numeric(0)
  obj <- ode_system(stiff_class, pars)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  expect_that(obj$has_jacobian, is_true())
  expect_that(obj$jacobian(y0, t0),
              is_identical_to(stiff_r_jacobian(y0, t0, pars)))
})

test_that("parameters", {
  pars <- numeric(0)
  obj <- ode_system(stiff_class, pars)
  expect_that(obj$get_pars(), is_identical_to(pars))

  ## Can hapilly set nonsense, because the underlying class does no
  ## checking (it has not actual parameters).  Better would be for it
  ## to throw, but we can do that with validators later.
  pars2 <- list(a=1, b=2)
  obj$set_pars(pars2)
  expect_that(obj$get_pars(), is_identical_to(pars2))
})

test_that("parameter validation", {
  pars <- 0.5
  obj <- ode_system(stiff_class, pars, validate=positive_scalar_numeric)

  ## Will throw on set:
  expect_that(obj$set_pars(-pars),         throws_error("Not positive"))
  expect_that(obj$set_pars(c(pars, pars)), throws_error("Not scalar"))
  expect_that(obj$set_pars(numeric(0)),    throws_error("Not scalar"))
  expect_that(obj$set_pars("pars"),        throws_error("Not numeric"))
  expect_that(obj$set_pars(list(pars)),    throws_error("Not numeric"))

  ## And on creation:
  expect_that(ode_system(stiff_class, -pars,
                         validate=positive_scalar_numeric),
              throws_error("Not positive"))
  expect_that(ode_system(stiff_class, c(pars, pars),
                         validate=positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(stiff_class, numeric(0),
                         validate=positive_scalar_numeric),
              throws_error("Not scalar"))
  ## Intercepted by initialiser
  expect_that(ode_system(stiff_class, "pars",
                         validate=positive_scalar_numeric),
              throws_error("not compatible"))
  expect_that(ode_system(stiff_class, list(pars),
                         validate=positive_scalar_numeric),
              throws_error("not compatible"))
})

test_that("copying", {
  pars <- 0.5
  obj <- ode_system(stiff_class, pars)
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
  obj <- ode_system(stiff_class, pars)
  f <- tempfile(fileext=".rds")
  saveRDS(obj, f)
  restored <- readRDS(f)
  expect_that(rodeint:::ptr_valid(restored$ptr), is_false())
  expect_that(rodeint:::ptr_address(restored$ptr), equals("0x0"))
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
  expect_that(rodeint:::ptr_valid(restored$ptr), is_true())
  expect_that(restored$get_pars(),
              is_identical_to(obj$get_pars()))
})

test_that("deSolve interface", {
  pars <- numeric(0)
  obj <- ode_system(stiff_class, pars)
  expect_that(obj$deSolve_info(),
              throws_error("Not yet supported"))
})

test_that("construction from deSolve type", {
  pars <- numeric(0)
  expect_that(ode_system(stiff_class, pars, deSolve_style=TRUE),
              throws_error("Only meaningful for R functions"))
})
