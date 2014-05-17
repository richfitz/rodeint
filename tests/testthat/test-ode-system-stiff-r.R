source("helper-rodeint.R")

context("ode_system (with jacobian, r)")

test_that("construction", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)
  expect_that(obj, is_a("ode_system"))
  expect_that(obj$type, is_identical_to("ode_system_stiff_r"))
})

test_that("show / print", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)
  expect_that(obj$show(), prints_text("ordinary differential equations"))
  expect_that(obj$show(), prints_text("Jacobian"))
  ## Detailed method:
  expect_that(obj$show(), not(prints_text("addr")))
  expect_that(obj$show(TRUE), prints_text("addr"))
  expect_that(obj$show(TRUE), prints_text("ode_system_stiff_r"))
})

test_that("derivatives", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  expect_that(obj$derivs(y0, t0),
              is_identical_to(stiff_r_derivs(y0, t0, pars)))
})

test_that("jacobian", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  expect_that(obj$has_jacobian, is_true())
  expect_that(obj$jacobian(y0, t0),
              is_identical_to(stiff_r_jacobian(y0, t0, pars)))
})

test_that("parameters", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)
  expect_that(obj$get_pars(), is_identical_to(pars))

  ## Can hapilly set nonsense:
  pars2 <- list(a=1, b=2)
  obj$set_pars(pars2)
  expect_that(obj$get_pars(), is_identical_to(pars2))
})

test_that("parameter validation", {
  pars <- 0.5
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian,
                    validate=positive_scalar_numeric)

  ## Will throw on set:
  expect_that(obj$set_pars(-pars),         throws_error("Not positive"))
  expect_that(obj$set_pars(c(pars, pars)), throws_error("Not scalar"))
  expect_that(obj$set_pars(numeric(0)),    throws_error("Not scalar"))
  expect_that(obj$set_pars("pars"),        throws_error("Not numeric"))
  expect_that(obj$set_pars(list(pars)),    throws_error("Not numeric"))

  ## And on creation:
  expect_that(ode_system(stiff_r_derivs, -pars,
                         jacobian=stiff_r_jacobian,
                         validate=positive_scalar_numeric),
              throws_error("Not positive"))
  expect_that(ode_system(stiff_r_derivs, c(pars, pars),
                         jacobian=stiff_r_jacobian,
                         validate=positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(stiff_r_derivs, numeric(0),
                         jacobian=stiff_r_jacobian,
                         validate=positive_scalar_numeric),
              throws_error("Not scalar"))
  expect_that(ode_system(stiff_r_derivs, "pars",
                         jacobian=stiff_r_jacobian,
                         validate=positive_scalar_numeric),
              throws_error("Not numeric"))
  expect_that(ode_system(stiff_r_derivs, list(pars),
                         jacobian=stiff_r_jacobian,
                         validate=positive_scalar_numeric),
              throws_error("Not numeric"))
})

test_that("copying", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)
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
  pars <- numeric(0)
  y0 <- c(0, 1)
  t0 <- 0.0
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)
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
  expect_that(restored$get_pars(),
              is_identical_to(obj$get_pars()))
})

test_that("deSolve interface", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs, pars, jacobian=stiff_r_jacobian)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  info <- obj$deSolve_info()
  expect_that(names(info),
              is_identical_to(c("func", "jacfunc",
                                "dllname", "initfunc", "initpar")))
  expect_that(info$func,     is_a("function"))
  expect_that(info$jacfunc,  is_a("function"))
  expect_that(info$dllname,  is_identical_to(NULL))
  expect_that(info$initfunc, is_identical_to(NULL))
  expect_that(info$initpar,  is_identical_to(NULL))

  expect_that(info$func(t0, y0, pars),
              is_identical_to(stiff_r_derivs_deSolve(t0, y0, pars)))
  expect_that(info$jacfunc(t0, y0, pars),
              is_identical_to(stiff_r_jacobian_deSolve(t0, y0, pars)))
})

test_that("construction from deSolve type", {
  pars <- numeric(0)
  obj <- ode_system(stiff_r_derivs_deSolve, pars,
                    jacobian=stiff_r_jacobian_deSolve,
                    deSolve_style=TRUE)
  expect_that(obj, is_a("ode_system"))

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0.0

  expect_that(obj$derivs(y0, t0),
              is_identical_to(stiff_r_derivs(y0, t0, pars)))
  expect_that(obj$jacobian(y0, t0),
              is_identical_to(stiff_r_jacobian(y0, t0, pars)))

  expect_that(obj$deSolve_info()$func(t0, y0, ignored),
              is_identical_to(stiff_r_derivs_deSolve(t0, y0, pars)))
  expect_that(obj$deSolve_info()$jacfunc(t0, y0, ignored),
              is_identical_to(stiff_r_jacobian_deSolve(t0, y0, pars)))
})
