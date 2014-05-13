source("helper-rodeint.R")

context("integrate (stiff)")

test_that("integrate_adaptive", {
  pars <- numeric(0)
  ode_r <- ode_system_stiff(stiff_r, pars)
  ode_cpp <- ode_system_stiff(stiff_cpp, pars)
  ode_class <- ode_system_stiff(stiff_class, pars)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0
  t1 <- 4
  dt0 <- 0.01
  save_state <- TRUE

  tolerance <- 1e-5

  ## Dummy stepper, not real
  s <- structure(NULL, class="stepper")

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), func=stiff_r_derivs_deSolve,
               jacfunc=stiff_r_jacobian_deSolve,
               atol=1e-8, rtol=1e-8)[-1,-1])

  ## TODO: Here, and in test-integrate.R, y_r -> y

  ## NOTE: These are quite different - would be good to see which is
  ## "correct".
  y_r <- integrate_adaptive(s, ode_r, y0, t0, t1, dt0)
  expect_that(y_r, is_a("numeric"))
  expect_that(y_r, equals(cmp, tolerance=tolerance))

  y_r_s <- integrate_adaptive(s, ode_r, y0, t0, t1, dt0, TRUE)
  expect_that(as.numeric(y_r_s), is_identical_to(y_r))
  expect_that(names(attributes(y_r_s)), equals(c("steps", "t", "y")))

  steps <- attr(y_r_s, "steps")
  tt    <- attr(y_r_s, "t")
  yy    <- attr(y_r_s, "y")

  expect_that(steps, is_a("numeric"))
  expect_that(tt,    is_a("numeric"))
  expect_that(yy,    is_a("matrix"))

  expect_that(length(tt), equals(steps + 1))
  expect_that(dim(yy),    equals(c(steps + 1, length(y0))))

  expect_that(tt[[1]],         is_identical_to(t0))
  expect_that(tt[[steps + 1]], is_identical_to(t1))

  expect_that(yy[1,],         is_identical_to(y0))
  expect_that(yy[steps + 1,], is_identical_to(y_r))

  ## Check the compiled version:
  expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0),
              is_identical_to(y_r))
  expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0, TRUE),
              is_identical_to(y_r_s))

  expect_that(integrate_adaptive(s, ode_class, y0, t0, t1, dt0),
              is_identical_to(y_r))
  expect_that(integrate_adaptive(s, ode_class, y0, t0, t1, dt0, TRUE),
              is_identical_to(y_r_s))
})
