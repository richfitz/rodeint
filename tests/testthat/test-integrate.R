source("helper-rodeint.R")

context("integrate")

test_that("integrate_simple", {
  ## First, test the basic integration function.  This is almost
  ## entirely tuning free.

  pars <- 0.5
  ode <- target_r(harmonic.oscillator, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt <- 0.1

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  ## Run with rodent:
  y1 <- integrate_simple(ode, y0, t0, t1, dt)
  expect_that(y1, is_a("numeric"))
  expect_that(y1, equals(cmp, tolerance=1e-5))

  ## Also return information about the run (seems not to have worked,
  ## may have been busted by adding attributes afterwards).
  y2 <- integrate_simple(ode, y0, t0, t1, dt, TRUE)
  expect_that(y2, equals(cmp, tolerance=1e-5, check.attributes=FALSE))
  expect_that(as.numeric(y2), is_identical_to(y1))
  expect_that(names(attributes(y2)), equals(c("steps", "t", "y")))

  steps <- attr(y2, "steps")
  tt    <- attr(y2, "t")
  yy    <- attr(y2, "y")

  expect_that(steps, is_a("numeric"))
  expect_that(tt,    is_a("numeric"))
  expect_that(yy,    is_a("matrix"))

  expect_that(length(tt), equals(steps + 1))
  expect_that(dim(yy),    equals(c(steps + 1, length(y0))))

  expect_that(tt[[1]],         is_identical_to(t0))
  expect_that(tt[[steps + 1]], is_identical_to(t1))

  expect_that(yy[1,],         is_identical_to(y0))
  expect_that(yy[steps + 1,], is_identical_to(y1))
})

test_that("integrate_const", {
  ## TODO: A couple of things to try here:
  ##   - t1 - t0 not a multiple of dt
  ##   - t1 < t0
  pars <- 0.5
  ode <- target_r(harmonic.oscillator, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.05

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  for (type in controlled_stepper_types()) {
    s <- controlled_stepper(type)
    y1 <- integrate_const(s, ode, y0, t0, t1, dt0)
    expect_that(y1, is_a("numeric"))
    expect_that(y1, equals(cmp, tolerance=1e-5))

    y2 <- integrate_const(s, ode, y0, t0, t1, dt0, TRUE)
    expect_that(y2, equals(cmp, tolerance=1e-5, check.attributes=FALSE))
    ## This fails for all three, which is extremely surprising...
    ##   expect_that(as.numeric(y2), is_identical_to(y1))
    ## This is because:
    ##   "However, if an observer is provided the step size will be
    ##    adjusted such that the algorithm always calculates x(t) at t
    ##    = t0 + n dt and calls the observer at that point."
    expect_that(names(attributes(y2)), equals(c("steps", "t", "y")))

    steps <- attr(y2, "steps")
    tt    <- attr(y2, "t")
    yy    <- attr(y2, "y")

    expect_that(steps, is_a("numeric"))
    expect_that(tt,    is_a("numeric"))
    expect_that(yy,    is_a("matrix"))

    expect_that(tt[[1]],         is_identical_to(t0))
    expect_that(tt[[steps + 1]], is_identical_to(t1))

    expect_that(yy[1,],         is_identical_to(y0))
    expect_that(yy[steps + 1,], is_identical_to(as.numeric(y2)))
  }
})

test_that("integrate_n_steps", {
  ## TODO: Try negative number of steps
  pars <- 0.5
  ode <- target_r(harmonic.oscillator, pars)

  y0 <- c(0, 1)
  t0 <- 0
  n  <- 10
  dt0 <- 0.05
  t1 <- t0 + n * dt0

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1),
                      wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  for (type in controlled_stepper_types()) {
    s <- controlled_stepper(type)
    y1 <- integrate_n_steps(s, ode, y0, t0, dt0, n)
    expect_that(y1, is_a("numeric"))
    expect_that(y1, equals(cmp, tolerance=1e-5))

    y2 <- integrate_n_steps(s, ode, y0, t0, dt0, n, TRUE)
    expect_that(y2, equals(cmp, tolerance=1e-5, check.attributes=FALSE))
    expect_that(names(attributes(y2)), equals(c("steps", "t", "y")))

    steps <- attr(y2, "steps")
    tt    <- attr(y2, "t")
    yy    <- attr(y2, "y")

    expect_that(steps, is_a("numeric"))
    expect_that(tt,    is_a("numeric"))
    expect_that(yy,    is_a("matrix"))

    expect_that(steps, equals(n))

    expect_that(tt[[1]],         is_identical_to(t0))
    expect_that(tt[[steps + 1]], is_identical_to(t1))

    expect_that(yy[1,],         is_identical_to(y0))
    expect_that(yy[steps + 1,], is_identical_to(as.numeric(y2)))
  }
})

test_that("integrate_adaptive", {
  ## TODO: Try zero dt
  pars <- 0.5
  ode <- target_r(harmonic.oscillator, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.01
  tol <- 1e-6

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), wrap.deSolve(harmonic.oscillator),
                      pars)[-1,-1])

  for (type in controlled_stepper_types()) {
    s <- controlled_stepper(type)

    ## Run with rodent:
    y1 <- integrate_adaptive(s, ode, y0, t0, t1, dt0)
    expect_that(y1, is_a("numeric"))
    expect_that(y1, equals(cmp, tolerance=1e-5))

    y2 <- integrate_adaptive(s, ode, y0, t0, t1, dt0, TRUE)
    expect_that(y2, equals(cmp, tolerance=1e-5, check.attributes=FALSE))
    expect_that(as.numeric(y2), is_identical_to(y1))
    expect_that(names(attributes(y2)), equals(c("steps", "t", "y")))

    steps <- attr(y2, "steps")
    tt    <- attr(y2, "t")
    yy    <- attr(y2, "y")

    expect_that(steps, is_a("numeric"))
    expect_that(tt,    is_a("numeric"))
    expect_that(yy,    is_a("matrix"))

    expect_that(length(tt), equals(steps + 1))
    expect_that(dim(yy),    equals(c(steps + 1, length(y0))))

    expect_that(tt[[1]],         is_identical_to(t0))
    expect_that(tt[[steps + 1]], is_identical_to(t1))

    expect_that(yy[1,],         is_identical_to(y0))
    expect_that(yy[steps + 1,], is_identical_to(y1))
  }
})

test_that("integrate_times", {
  ## TODO: check times of length 0, 1, fails
  pars <- 0.5
  ode <- target_r(harmonic.oscillator, pars)

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

  for (type in controlled_stepper_types()) {
    s <- controlled_stepper(type)

    ## Run with rodent:
    y2 <- integrate_times(s, ode, y0, times, dt0)
    expect_that(y2, equals(cmp[nrow(cmp),], tolerance=1e-5,
                           check.attributes=FALSE))
    expect_that(names(attributes(y2)), equals(c("steps", "t", "y")))

    steps <- attr(y2, "steps")
    tt    <- attr(y2, "t")
    yy    <- attr(y2, "y")

    expect_that(steps, is_a("numeric"))
    expect_that(tt,    is_a("numeric"))
    expect_that(yy,    is_a("matrix"))

    expect_that(steps, is_more_than(length(tt) - 1))
    expect_that(nrow(yy), equals(length(times)))

    expect_that(tt,    is_identical_to(times))
    expect_that(yy,    equals(cmp, tolerance=1e-5))
  }
})
