## More detailed testing on the integrate_const function
source("helper-rodeint.R")

context("integrate_const")

test_that("Time ends at multiple of dt", {
  pars <- 0.5
  ode_system <- ode_system(harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  t1 <- t0 + n * dt

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_const(s, ode_system, y0, t0, t1, dt)
      expect_that(y_r, is_a("numeric"))

      y_r_s <- integrate_const(s, ode_system, y0, t0, t1, dt, TRUE)
      if (category == "basic") {
        expect_that(as.numeric(y_r_s), is_identical_to(y_r))
      } else {
        expect_that(as.numeric(y_r_s), equals(y_r, tolerance=1e-6))
      }

      t_expected <- seq(t0, t1, by=dt)
      expect_that(attr(y_r_s, "t"), is_identical_to(t_expected))
      expect_that(attr(y_r_s, "steps"), equals(n))
    }
  }
})

test_that("Time ends in the middle of a step", {
  pars <- 0.5
  ode_system <- ode_system(harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  t1 <- t0 + (n + 0.33) * dt

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_const(s, ode_system, y0, t0, t1, dt)
      expect_that(y_r, is_a("numeric"))

      ## Run the observed stepper out to t1, and then also dt *past* t1:
      y_r_s_1 <- integrate_const(s, ode_system, y0, t0, t1,    dt, TRUE)
      y_r_s_2 <- integrate_const(s, ode_system, y0, t0, t1+dt, dt, TRUE)

      t_expected <- seq(t0, t1, by=dt)
      expect_that(attr(y_r_s_1, "t"), is_identical_to(t_expected))
      expect_that(attr(y_r_s_1, "steps"), equals(length(t_expected) - 1))
      expect_that(attr(y_r_s_2, "steps"), equals(length(t_expected)))

      ## Now, the estimate y_r should be between the last two points:
      expect_that(y_r,
                  is_in_range(last_row(attr(y_r_s_1, "y")),
                              last_row(attr(y_r_s_2, "y"))))

      ## Graphical illustration of what is going on:
      if (interactive()) {
        matplot(attr(y_r_s_2, "t"), attr(y_r_s_2, "y"), type="o",
                pch=1, cex=.5, xlab="t", ylab="y",
                main=paste(category, type, sep=" / "))
        matpoints(attr(y_r_s_1, "t"), attr(y_r_s_1, "y"), pch=19, cex=.5)
        points(rep(t1, 2), y_r, pch=4, col=1:2, cex=2)
      }
    }
  }
})

## There are two ways that time can run "backwards", though I'm not
## sure whether this always makes sense.  Details are in src/util.cpp
## for now, but need adding to the documentation.  The behaviours
## should be unsurprising.
##
## tl;dr sign(t1 - t0) == sign(dt) or world of pain
test_that("Time runs backwards", {
  pars <- 0.5
  ode_system <- ode_system(harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  ## This is the correct setup for backward time system evolution --
  ## dt has the same sign as (t1 - t0)
  dt <- -0.05
  t1 <- t0 + (n + .33) * dt

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_const(s, ode_system, y0, t0, t1, dt)
      y_r_s_1 <- integrate_const(s, ode_system, y0, t0, t1,    dt, TRUE)
      y_r_s_2 <- integrate_const(s, ode_system, y0, t0, t1+dt, dt, TRUE)

      expect_that(attr(y_r_s_1, "t"),
                  equals(seq(t0, t1, by=dt)))

      if (interactive()) {
        matplot(attr(y_r_s_2, "t"), attr(y_r_s_2, "y"), type="o",
                pch=1, cex=.5, xlab="t", ylab="y",
                main=paste(category, type, sep=" / "))
        matpoints(attr(y_r_s_1, "t"), attr(y_r_s_1, "y"), pch=19, cex=.5)
        points(rep(t1, 2), y_r, pch=4, col=1:2, cex=2)
      }

      ## Check the corner cases -- running time in the wrong direction
      ## for [t0, t1] is an error.
      expect_that(integrate_const(s, ode_system, y0, t0, t1, -dt),
                  throws_error("dt has the wrong sign"))
      expect_that(integrate_const(s, ode_system, y0, t1, t0, dt),
                  throws_error("dt has the wrong sign"))

      ## More corner cases: when t0 = t1, the sign of dt does not matter
      expect_that(integrate_const(s, ode_system, y0, t0, t0, dt),
                  is_identical_to(y0))
      expect_that(integrate_const(s, ode_system, y0, t0, t0, -dt),
                  is_identical_to(y0))

      ## More corner cases: when dt = 0 it is always an error,
      ## regardless of the sign of t0 / t1
      expect_that(integrate_const(s, ode_system, y0, t0, t1, 0),
                  throws_error("dt cannot be zero"))
      expect_that(integrate_const(s, ode_system, y0, t1, t0, 0),
                  throws_error("dt cannot be zero"))
      expect_that(integrate_const(s, ode_system, y0, t0, t0, 0),
                  throws_error("dt cannot be zero"))
    }
  }
})

test_that("Argument handling for errored input", {
  pars <- 0.5
  ode_system <- ode_system(harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  t1 <- t0 + n * dt
  s <- make_stepper("basic", "runge_kutta4")

  ## I often pass in stats::dt instead of some variable dt because I am
  ## a moron.  That generates the unhelpful error message "not
  ## compatible with requested type", but never says what variable
  ## we're talking about.
  expect_that(integrate_const(s, ode_system, y0, t0, t1, stats::dt),
              throws_error("dt must be numeric"))

  expect_that(integrate_const(s, ode_system, y0, "t0", t1, dt),
              throws_error("t0 must be numeric"))
  expect_that(integrate_const(s, ode_system, y0, t0, "t1", dt),
              throws_error("t1 must be numeric"))

  ## Integers will be accepted in place of numerics:
  expect_that(integrate_const(s, ode_system, y0, 0L, t1, dt),
              is_identical_to(integrate_const(s, ode_system, y0,
                                              0.0, t1, dt)))
  expect_that(integrate_const(s, ode_system, y0, t0, 1L, dt),
              is_identical_to(integrate_const(s, ode_system, y0,
                                              t0, 1.0, dt)))
  expect_that(integrate_const(s, ode_system, y0, t0, t1, 1L),
              is_identical_to(integrate_const(s, ode_system, y0,
                                              t0, t1, 1.0)))
})
