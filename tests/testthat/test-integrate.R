source("helper-rodeint.R")

context("integrate")

## Once we get the non-adaptive integrators working here, there's
## potentially a bunch more to work on -- they do have slightly
## different behaviours.
##
## Also we'll need to tune tolerances for different algorithms --
## euler won't do as well as rk4, for example!
##
## Might be worth getting the analytical solution (Mathematica?) so
## that we can compare that too.
##
## OK, especially as we now have *three* target types, and as almost
## all the actual testing is spent in this file we might have to break
## this up into five functions - one for each integration type.

## There is more work to do on accuracy later, but for now this is
## probably enough.  We're not really looking to verify that the
## integrators work -- that's for odeint to do, and there is a big
## test suite there.
expected_tolerance <- function(type) {
  switch(type,
         euler=0.03, # such inaccuracy
         modified_midpoint=1e-4,
         1e-5)
}

test_that("integrate_const", {
  ## TODO: A couple of things to try here:
  ##   - t1 - t0 not a multiple of dt
  ##   - t1 < t0
  ##   - less obscure message if dt is stats::dt (or other wrong type)
  pars <- 0.5
  ode_r <- target_r(harmonic.oscillator, pars)
  ode_cpp <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.05

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  for (category in c("basic", "controlled")) {
    for (type in stepper_types(category)) {
      tolerance <- expected_tolerance(type)
      s <- make_stepper(category, type)
      y_r <- ode_r$integrate_const(s, y0, t0, t1, dt0)
      expect_that(y_r, is_a("numeric"))
      expect_that(y_r, equals(cmp, tolerance=tolerance))

      y_r_s <- ode_r$integrate_const(s, y0, t0, t1, dt0, TRUE)
      expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                                check.attributes=FALSE))
      ## This fails for all three, which is extremely surprising...
      ##   expect_that(as.numeric(y_r_s), is_identical_to(y_r))
      ## This is because:
      ##   "However, if an observer is provided the step size will be
      ##    adjusted such that the algorithm always calculates x(t) at t
      ##    = t0 + n dt and calls the observer at that point."
      expect_that(names(attributes(y_r_s)), equals(c("steps", "t", "y")))

      steps <- attr(y_r_s, "steps")
      tt    <- attr(y_r_s, "t")
      yy    <- attr(y_r_s, "y")

      expect_that(steps, is_a("numeric"))
      expect_that(tt,    is_a("numeric"))
      expect_that(yy,    is_a("matrix"))

      expect_that(tt[[1]],         is_identical_to(t0))
      expect_that(tt[[steps + 1]], is_identical_to(t1))

      expect_that(yy[1,],         is_identical_to(y0))
      expect_that(yy[steps + 1,], is_identical_to(as.numeric(y_r_s)))

      ## Check the compiled version:
      expect_that(ode_cpp$integrate_const(s, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(ode_cpp$integrate_const(s, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      ## Check the alternative syntax:
      expect_that(integrate_const(s, ode_r, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_const(s, ode_r, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_const(s, ode_cpp, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_const(s, ode_cpp, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("integrate_n_steps", {
  ## TODO: Try negative number of steps
  pars <- 0.5
  ode_r <- target_r(harmonic.oscillator, pars)
  ode_cpp <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 0
  n  <- 10
  dt0 <- 0.05
  t1 <- t0 + n * dt0

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1),
                      wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  for (category in c("basic", "controlled")) {
    for (type in stepper_types(category)) {
      tolerance <- expected_tolerance(type)
      s <- make_stepper(category, type)
      y_r <- ode_r$integrate_n_steps(s, y0, t0, dt0, n)
      expect_that(y_r, is_a("numeric"))
      expect_that(y_r, equals(cmp, tolerance=tolerance))

      y_r_s <- ode_r$integrate_n_steps(s, y0, t0, dt0, n, TRUE)
      expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                                check.attributes=FALSE))
      expect_that(names(attributes(y_r_s)), equals(c("steps", "t", "y")))

      steps <- attr(y_r_s, "steps")
      tt    <- attr(y_r_s, "t")
      yy    <- attr(y_r_s, "y")

      expect_that(steps, is_a("numeric"))
      expect_that(tt,    is_a("numeric"))
      expect_that(yy,    is_a("matrix"))

      expect_that(steps, equals(n))

      expect_that(tt[[1]],         is_identical_to(t0))
      expect_that(tt[[steps + 1]], is_identical_to(t1))

      expect_that(yy[1,],         is_identical_to(y0))
      expect_that(yy[steps + 1,], is_identical_to(as.numeric(y_r_s)))

      ## Check the compiled version:
      expect_that(ode_cpp$integrate_n_steps(s, y0, t0, dt0, n),
                  is_identical_to(y_r))
      expect_that(ode_cpp$integrate_n_steps(s, y0, t0, dt0, n, TRUE),
                  is_identical_to(y_r_s))

      ## Check the alternative syntax:
      expect_that(integrate_n_steps(s, ode_r, y0, t0, dt0, n),
                  is_identical_to(y_r))
      expect_that(integrate_n_steps(s, ode_r, y0, t0, dt0, n, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_n_steps(s, ode_cpp, y0, t0, dt0, n),
                  is_identical_to(y_r))
      expect_that(integrate_n_steps(s, ode_cpp, y0, t0, dt0, n, TRUE),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("integrate_adaptive", {
  ## TODO: Try zero dt
  pars <- 0.5
  ode_r <- target_r(harmonic.oscillator, pars)
  ode_cpp <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.01
  tol <- 1e-6

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  for (category in c("basic", "controlled")) {
    for (type in stepper_types(category)) {
      tolerance <- expected_tolerance(type)
      s <- make_stepper(category, type)

      ## run with rodeint:
      y_r <- ode_r$integrate_adaptive(s, y0, t0, t1, dt0)
      expect_that(y_r, is_a("numeric"))
      expect_that(y_r, equals(cmp, tolerance=tolerance))

      y_r_s <- ode_r$integrate_adaptive(s, y0, t0, t1, dt0, TRUE)
      expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                                check.attributes=FALSE))
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
      expect_that(ode_cpp$integrate_adaptive(s, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(ode_cpp$integrate_adaptive(s, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      ## Check the alternative syntax:
      expect_that(integrate_adaptive(s, ode_r, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_adaptive(s, ode_r, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("integrate_times", {
  ## TODO: check times of length 0, 1, fails
  pars <- 0.5
  ode_r <- target_r(harmonic.oscillator, pars)
  ode_cpp <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.01
  tol <- 1e-6

  # Some randomly spaced times between t0 and t1:
  set.seed(1)
  times <- sort(c(t0, t1, runif(5, t0, t1)))

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, times, wrap.deSolve(harmonic.oscillator),
                      pars)[,-1])

  for (category in c("basic", "controlled")) {
    for (type in stepper_types(category)) {
      tolerance <- expected_tolerance(type)
      s <- make_stepper(category, type)

      ## run with rodeint:
      y_r_s <- ode_r$integrate_times(s, y0, times, dt0)
      expect_that(y_r_s, equals(cmp[nrow(cmp),], tolerance=tolerance,
                                check.attributes=FALSE))
      expect_that(names(attributes(y_r_s)), equals(c("steps", "t", "y")))

      steps <- attr(y_r_s, "steps")
      tt    <- attr(y_r_s, "t")
      yy    <- attr(y_r_s, "y")

      expect_that(steps, is_a("numeric"))
      expect_that(tt,    is_a("numeric"))
      expect_that(yy,    is_a("matrix"))

      expect_that(steps, is_more_than(length(tt) - 1))
      expect_that(nrow(yy), equals(length(times)))

      expect_that(tt,    is_identical_to(times))
      expect_that(yy,    equals(cmp, tolerance=tolerance))

      ## Check the compiled version:
      expect_that(ode_cpp$integrate_times(s, y0, times, dt0),
                  is_identical_to(y_r_s))

      ## Check the alternative syntax:
      expect_that(integrate_times(s, ode_r, y0, times, dt0),
                  is_identical_to(y_r_s))
      expect_that(integrate_times(s, ode_cpp, y0, times, dt0),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("integrate_simple", {
  pars <- 0.5
  ode_r <- target_r(harmonic.oscillator, pars)
  ode_cpp <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt <- 0.1

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  tolerance <- expected_tolerance("runge_kutta_dopri5")

  ## run with rodeint:
  y_r <- ode_r$integrate_simple(y0, t0, t1, dt)
  expect_that(y_r, is_a("numeric"))
  expect_that(y_r, equals(cmp, tolerance=tolerance))

  y_r_s <- ode_r$integrate_simple(y0, t0, t1, dt, TRUE)
  expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                            check.attributes=FALSE))
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

  ## Check the compiled version
  expect_that(ode_cpp$integrate_simple(y0, t0, t1, dt),
              is_identical_to(y_r))
  expect_that(ode_cpp$integrate_simple(y0, t0, t1, dt, TRUE),
              is_identical_to(y_r_s))

  ## Check the alternative syntax:
  expect_that(integrate_simple(ode_r, y0, t0, t1, dt),
              is_identical_to(y_r))
  expect_that(integrate_simple(ode_r, y0, t0, t1, dt, TRUE),
              is_identical_to(y_r_s))

  expect_that(integrate_simple(ode_cpp, y0, t0, t1, dt),
              is_identical_to(y_r))
  expect_that(integrate_simple(ode_cpp, y0, t0, t1, dt, TRUE),
              is_identical_to(y_r_s))
})
