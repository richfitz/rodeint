source("helper-rodeint.R")

context("ode_system_stiff (r)")

test_that("construction", {
  pars <- numeric(0)
  obj <- ode_system_stiff(stiff_cpp, pars)
  expect_that(obj, is_a("ode_system_stiff"))
  expect_that(obj$type, is_identical_to("ode_system_stiff_cpp"))
  expect_that(obj$ptr <- obj$ptr,
              throws_error("read-only"))
})

test_that("derivatives", {
  pars <- numeric(0)
  obj <- ode_system_stiff(stiff_cpp, pars)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  expect_that(obj$derivs(y0, t0),
              is_identical_to(stiff_r_derivs(y0, t0, pars)))
  expect_that(obj$jacobian(y0, t0),
              is_identical_to(stiff_r_jacobian(y0, t0, pars)))
})

test_that("parameters", {
  pars <- numeric(0)
  obj <- ode_system_stiff(stiff_cpp, pars)
  expect_that(obj$get_pars(), is_identical_to(pars))

  ## In contrast with ode_system_r, this will throw:
  pars2 <- list(a=1, b=2)
  expect_that(obj$set_pars(pars2),
              throws_error("not compatible with requested type"))
  expect_that(obj$set_pars(c(1, 2)),
              throws_error("Incorrect length"))
  expect_that(obj$set_pars(1),
              throws_error("Incorrect length"))
  ## Did not affect previous:
  expect_that(obj$get_pars(), is_identical_to(pars))
})

test_that("parameter validation", {
  pars <- 0.5
  obj <- ode_system_stiff(stiff_cpp, pars, positive_scalar_numeric)

  ## Will throw on set:
  expect_that(obj$set_pars(-pars),         throws_error("Not positive"))
  expect_that(obj$set_pars(c(pars, pars)), throws_error("Not scalar"))
  expect_that(obj$set_pars(numeric(0)),    throws_error("Not scalar"))
  expect_that(obj$set_pars("pars"),        throws_error("Not numeric"))
  expect_that(obj$set_pars(list(pars)),    throws_error("Not numeric"))

  ## And on creation:
  expect_that(ode_system_stiff(stiff_cpp, -pars,
                         positive_scalar_numeric),
              throws_error("Not positive"))
  expect_that(ode_system_stiff(stiff_cpp, c(pars, pars),
                         positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system_stiff(stiff_cpp, numeric(0),
                         positive_scalar_numeric),
              throws_error("Not scalar"))
  ## Intercepted by initialiser:
  expect_that(ode_system_stiff(stiff_cpp, "pars",
                         positive_scalar_numeric),
              throws_error("not compatible"))
  expect_that(ode_system_stiff(stiff_cpp, list(pars),
                         positive_scalar_numeric),
              throws_error("not compatible"))
})

test_that("copying", {
  pars <- 0.5
  obj <- ode_system_stiff(stiff_cpp, pars)
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
  pars <- numeric(0)
  obj <- ode_system_stiff(stiff_cpp, pars)
  expect_that(obj$deSolve_info(),
              throws_error("Not yet supported"))
})

test_that("construction from deSolve type", {
  pars <- numeric(0)
  expect_that(ode_system_stiff(stiff_cpp, pars, deSolve_style=TRUE),
              throws_error("Only meaningful for R functions"))
})
