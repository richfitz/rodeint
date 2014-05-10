source("helper-rodeint.R")

context("integrate_times")

test_that("Time are multiples of dt", {
  pars <- 0.5
  target <- target(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  times <- seq(t0, by=dt, length.out=n+1)

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_times(s, target, y0, times, dt)
      expect_that(y_r, is_a("matrix"))
      expect_that(attr(y_r, "t"), is_identical_to(times))
      expect_that(attr(y_r, "y"), is_identical_to(last_row(y_r)))

      if (category == "basic") {
        ## I do not know why this is the case:
        expect_that(attr(y_r, "steps"), equals(n+2))
      } else {
        expect_that(attr(y_r, "steps"), not(is_more_than(n)))
      }
    }
  }
})

test_that("Time ends in the middle of a step", {
  pars <- 0.5
  target <- target(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  times <- seq(t0, by=dt, length.out=n)
  times <- c(times, last(times) + dt * .33)

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_times(s, target, y0, times, dt)
      expect_that(y_r, is_a("matrix"))
      expect_that(attr(y_r, "t"), is_identical_to(times))

      if (category == "basic") {
        ## I do not know why this is the case:
        expect_that(attr(y_r, "steps"), equals(n+2))
      } else {
        expect_that(attr(y_r, "steps"), not(is_more_than(n)))
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
  target <- target(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  ## This is the correct setup for backward time system evolution --
  ## dt has the same sign as (t1 - t0)
  dt <- -0.05
  times <- seq(t0, by=dt, length.out=n)
  times <- c(times, last(times) + dt * .33)

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_times(s, target, y0, times, dt)
      expect_that(attr(y_r, "y"), is_identical_to(last_row(y_r)))

      ## TODO: The controlled versions do a shithouse job here.
      ## Surprising given that the adaptive ones worked I believe.
      if (interactive()) {
        matplot(attr(y_r, "t"), y_r, type="o",
                pch=1, cex=.5, xlab="t", ylab="y",
                main=paste(category, type, sep=" / "))
        points(rep(last(times), 2), attr(y_r, "y"), pch=4, col=1:2, cex=2)
      }

      ## Check the corner cases -- running time in the wrong direction
      ## for [times] is an error.
      expect_that(integrate_times(s, target, y0, times, -dt),
                  throws_error("dt has the wrong sign"))
      expect_that(integrate_times(s, target, y0, rev(times), dt),
                  throws_error("dt has the wrong sign"))

      ## More corner cases: when t0 = t1, the sign of dt does not matter
      y0_nomove <- rbind(y0, y0, deparse.level=0)
      attr(y0_nomove, "steps") <- 0
      attr(y0_nomove, "t") <- c(t0, t0)
      attr(y0_nomove, "y") <- y0

      expect_that(integrate_times(s, target, y0, c(t0, t0), dt),
                  is_identical_to(y0_nomove))
      expect_that(integrate_times(s, target, y0, c(t0, t0), -dt),
                  is_identical_to(y0_nomove))

      ## More corner cases: when dt = 0 it is always an error,
      ## regardless of the sign of t0 / t1
      expect_that(integrate_times(s, target, y0, times, 0),
                  throws_error("dt cannot be zero"))
      expect_that(integrate_times(s, target, y0, rev(times), 0),
                  throws_error("dt cannot be zero"))
      expect_that(integrate_times(s, target, y0, c(t0, t0), 0),
                  throws_error("dt cannot be zero"))
    }
  }
})

test_that("Argument handling for errored input", {
  pars <- 0.5
  target <- target(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  times <- seq(t0, by=dt, length.out=n)
  times <- c(times, last(times) + dt * .33)

  s <- make_stepper("basic", "runge_kutta4")

  ## I often pass in stats::dt instead of some variable dt because I am
  ## a moron.  That generates the unhelpful error message "not
  ## compatible with requested type", but never says what variable
  ## we're talking about.
  expect_that(integrate_times(s, target, y0, times, stats::dt),
              throws_error("dt must be numeric"))

  expect_that(integrate_times(s, target, y0, "times", dt),
              throws_error("times must be numeric"))

  ## Integers will be accepted in place of numerics:
  expect_that(integrate_times(s, target, y0, times, 1L),
              is_identical_to(integrate_times(s, target, y0, times, 1L)))

  ## We refuse to integrate with an empty time vector:
  expect_that(integrate_times(s, target, y0, numeric(0), 1L),
              throws_error("Must provide at least two times"))
  ## TODO: Decide if the case below would be nice to work, retuning a
  ## 1 row matrix.  I think either behaviour is reasonable.
  expect_that(integrate_times(s, target, y0, t0, 1L),
              throws_error("Must provide at least two times"))

  ## Duplicate times should be totally fine:
  i <- 5
  j <- c(1:i, i:length(times))
  y2 <- integrate_times(s, target, y0, times[j], dt)
  cmp <- integrate_times(s, target, y0, times, dt)
  expect_that(y2[,], # drops attributes :-/
              is_identical_to(as.matrix(cmp)[j,]))
  expect_that(attr(y2, "t"), is_identical_to(times[j]))
  expect_that(attr(y2, "y"), is_identical_to(attr(cmp, "y")))
  expect_that(attr(y2, "steps"), is_identical_to(attr(cmp, "steps")))

  ## But an unsorted vector should throw an error
  i <- seq_along(times)
  i[4:6] <- 6:4
  expect_that(integrate_times(s, target, y0, times[i], dt),
              throws_error("Times must be sorted (increasing)",
                           fixed=TRUE))
  expect_that(integrate_times(s, target, y0, rev(times[i]), dt),
              throws_error("dt has the wrong sign",
                           fixed=TRUE))
  expect_that(integrate_times(s, target, y0, times[i], -dt),
              throws_error("dt has the wrong sign",
                           fixed=TRUE))
  expect_that(integrate_times(s, target, y0, rev(times[i]), -dt),
              throws_error("Times must be sorted (decreasing)",
                           fixed=TRUE))
})
