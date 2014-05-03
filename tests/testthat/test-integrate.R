source("helper-rodeint.R")

context("integrate")

test_that("integrate", {
  ## First, test the basic integration function.  This is almost
  ## entirely tuning free.
  integrate <- rodeint:::integrate

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
  y1 <- integrate(ode$ptr, y0, t0, t1, dt)
  expect_that(y1, is_a("numeric"))
  expect_that(y1, equals(cmp, tolerance=1e-5))

  ## Also return information about the run (seems not to have worked,
  ## may have been busted by adding attributes afterwards).
  y2 <- integrate(ode$ptr, y0, t0, t1, dt, TRUE)
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

test_that("integrate_adaptive", {
  integrate_adaptive <- rodeint:::integrate_adaptive

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

  for (type in rodeint:::controlled_stepper_types()) {
    s <- controlled_stepper(type)

    ## Run with rodent:
    y1 <- integrate_adaptive(s$ptr, ode$ptr, y0, t0, t1, dt0)
    expect_that(y1, is_a("numeric"))
    expect_that(y1, equals(cmp, tolerance=1e-5))

    y2 <- integrate_adaptive(s$ptr, ode$ptr, y0, t0, t1, dt0, TRUE)
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
