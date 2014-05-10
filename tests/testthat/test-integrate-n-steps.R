source("helper-rodeint.R")

context("integrate_n_steps")

test_that("Time ends at multiple of dt", {
  pars <- 0.5
  target <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_n_steps(s, target, y0, t0, dt, n)
      expect_that(y_r, is_a("numeric"))

      y_r_s <- integrate_n_steps(s, target, y0, t0, dt, n, TRUE)
      if (category == "basic") {
        expect_that(as.numeric(y_r_s), is_identical_to(y_r))
      } else {
        expect_that(as.numeric(y_r_s), equals(y_r, tolerance=1e-6))
      }

      t_expected <- seq(t0, by=dt, length.out=n + 1)
      expect_that(attr(y_r_s, "t"), is_identical_to(t_expected))
      expect_that(attr(y_r_s, "steps"), equals(n))
    }
  }
})

test_that("Time runs backwards", {
  pars <- 0.5
  target <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- -0.05

  for (category in stepper_categories()) {
    for (type in stepper_types(category)) {
      s <- make_stepper(category, type)

      y_r <- integrate_n_steps(s, target, y0, t0, dt, n)
      y_r_s <- integrate_n_steps(s, target, y0, t0, dt, n, TRUE)
      expect_that(attr(y_r_s, "t"),
                  equals(seq(t0, by=dt, length.out=n+1)))

      ## No backward time confusion for this method -- there is
      ## nothing for dt.  But negative numbers of steps, while rarer,
      ## still cause a problem because of type conversion.
      expect_that(integrate_n_steps(s, target, y0, t0, dt, -1L),
                  throws_error("n must be nonnegative"))

      ## If n = 0, check output = input, regardless of sign of dt:
      expect_that(integrate_n_steps(s, target, y0, t0, dt, 0),
                  is_identical_to(y0))
      expect_that(integrate_n_steps(s, target, y0, t0, -dt, 0),
                  is_identical_to(y0))

      ## More corner cases: when dt = 0 it is always an error,
      ## regardless of how many steps are requested.
      expect_that(integrate_n_steps(s, target, y0, t0, 0, 1L),
                  throws_error("dt cannot be zero"))
      expect_that(integrate_n_steps(s, target, y0, t0, 0, 0L),
                  throws_error("dt cannot be zero"))
    }
  }
})

test_that("Argument handling for errored input", {
  pars <- 0.5
  target <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  s <- make_stepper("basic", "runge_kutta4")

  ## I often pass in stats::dt instead of some variable dt because I am
  ## a moron.  That generates the unhelpful error message "not
  ## compatible with requested type", but never says what variable
  ## we're talking about.
  expect_that(integrate_n_steps(s, target, y0, t0, stats::dt, n),
              throws_error("dt must be numeric"))

  expect_that(integrate_n_steps(s, target, y0, "t0", dt, n),
              throws_error("t0 must be numeric"))
  expect_that(integrate_n_steps(s, target, y0, t0, "dt", n),
              throws_error("dt must be numeric"))
  expect_that(integrate_n_steps(s, target, y0, t0, dt, "n"),
              throws_error("n must be integer"))

  ## Integers will be accepted in place of numerics:
  expect_that(integrate_n_steps(s, target, y0, 0L, dt, n),
              is_identical_to(integrate_n_steps(s, target, y0, 0.0, dt, n)))
  expect_that(integrate_n_steps(s, target, y0, t0, 1L, n),
              is_identical_to(integrate_n_steps(s, target, y0, t0, 1L, n)))
  ## And numerics in place of integers for n
  expect_that(integrate_n_steps(s, target, y0, t0, dt, 20.0),
              is_identical_to(integrate_n_steps(s, target, y0, t0, dt, 20.0)))
  ## But not if they won't convert nicely to an integer
  expect_that(integrate_n_steps(s, target, y0, t0, dt, 20.1),
              throws_error("n must be integer"))
})
