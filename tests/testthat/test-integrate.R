source("helper-rodeint.R")

context("integrate")

## There is more work to do on accuracy later, but for now this is
## probably enough.  We're not really looking to verify that the
## integrators work -- that's for odeint to do, and there is a big
## test suite there.
expected_tolerance <- function(algorithm) {
  switch(algorithm,
         euler=0.03, # such inaccuracy
         modified_midpoint=1e-4,
         1e-5)
}

test_that("integrate_const", {
  pars <- 0.5
  ode_r <- ode_system(harmonic_oscillator_r, pars)
  ode_cpp <- ode_system(harmonic_oscillator_cpp, pars)
  ode_class <- ode_system(harmonic_oscillator_class, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.05

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), harmonic_oscillator_deSolve,
                      pars)[-1,-1])

  for (category in stepper_categories()) {
    for (algorithm in stepper_algorithms(category)) {
      tolerance <- expected_tolerance(algorithm)
      s <- make_stepper(category, algorithm)
      y_r <- integrate_const(s, ode_r, y0, t0, t1, dt0)
      expect_that(y_r, is_a("numeric"))
      expect_that(y_r, equals(cmp, tolerance=tolerance))

      y_r_s <- integrate_const(s, ode_r, y0, t0, t1, dt0, TRUE)
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

      expect_that(tt[[1]],      is_identical_to(t0))
      expect_that(yy[1,],       is_identical_to(y0))
      expect_that(last(tt),     is_identical_to(t1))
      expect_that(last_row(yy), is_identical_to(as.numeric(y_r_s)))

      ## Check the compiled version:
      expect_that(integrate_const(s, ode_cpp, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_const(s, ode_cpp, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_const(s, ode_class, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_const(s, ode_class, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      if (category == "dense") {
        expect_that(steps, is_less_than(length(tt)))
      } else {
        expect_that(steps, equals(length(tt) - 1L))
      }
    }
  }
})

test_that("integrate_n_steps", {
  pars <- 0.5
  ode_r <- ode_system(harmonic_oscillator_r, pars)
  ode_cpp <- ode_system(harmonic_oscillator_cpp, pars)
  ode_class <- ode_system(harmonic_oscillator_class, pars)

  y0 <- c(0, 1)
  t0 <- 0
  n  <- 10
  dt0 <- 0.05
  t1 <- t0 + n * dt0

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1),
                      harmonic_oscillator_deSolve,
                      pars)[-1,-1])

  for (category in stepper_categories()) {
    for (algorithm in stepper_algorithms(category)) {
      tolerance <- expected_tolerance(algorithm)
      s <- make_stepper(category, algorithm)
      y_r <- integrate_n_steps(s, ode_r, y0, t0, dt0, n)
      expect_that(y_r, is_a("numeric"))
      if (category != "dense") { # TODO: FIXME
        expect_that(y_r, equals(cmp, tolerance=tolerance))
      }

      y_r_s <- integrate_n_steps(s, ode_r, y0, t0, dt0, n, TRUE)
      if (category != "dense") { # TODO: FIXME
        expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                                  check.attributes=FALSE))
      }
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

      ## TODO: weird
      if (!(category == "dense" && algorithm == "bulirsch_stoer")) {
        expect_that(yy[1,],         is_identical_to(y0))
      }
      if (category != "dense") { # TODO: FIXME
        expect_that(yy[steps + 1,], is_identical_to(as.numeric(y_r_s)))
      }

      ## Check the compiled version:
      expect_that(integrate_n_steps(s, ode_cpp, y0, t0, dt0, n),
                  is_identical_to(y_r))
      expect_that(integrate_n_steps(s, ode_cpp, y0, t0, dt0, n, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_n_steps(s, ode_class, y0, t0, dt0, n),
                  is_identical_to(y_r))
      expect_that(integrate_n_steps(s, ode_class, y0, t0, dt0, n, TRUE),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("integrate_adaptive", {
  pars <- 0.5
  ode_r <- ode_system(harmonic_oscillator_r, pars)
  ode_cpp <- ode_system(harmonic_oscillator_cpp, pars)
  ode_class <- ode_system(harmonic_oscillator_class, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.01

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, c(t0, t1), harmonic_oscillator_deSolve,
                      pars)[-1,-1])

  for (category in stepper_categories()) {
    for (algorithm in stepper_algorithms(category)) {
      tolerance <- expected_tolerance(algorithm)
      s <- make_stepper(category, algorithm)

      ## run with rodeint:
      y_r <- integrate_adaptive(s, ode_r, y0, t0, t1, dt0)
      expect_that(y_r, is_a("numeric"))
      expect_that(y_r, equals(cmp, tolerance=tolerance))

      y_r_s <- integrate_adaptive(s, ode_r, y0, t0, t1, dt0, TRUE)
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
      expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_adaptive(s, ode_class, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_adaptive(s, ode_class, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("integrate_times", {
  pars <- 0.5
  ode_r <- ode_system(harmonic_oscillator_r, pars)
  ode_cpp <- ode_system(harmonic_oscillator_cpp, pars)
  ode_class <- ode_system(harmonic_oscillator_class, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.01

  # Some randomly spaced times between t0 and t1:
  set.seed(1)
  times <- sort(c(t0, t1, runif(5, t0, t1)))

  ## Here is the solution from deSolve, using lsoda:
  cmp <- unname(lsoda(y0, times, harmonic_oscillator_deSolve,
                      pars)[,-1])

  for (category in stepper_categories()) {
    for (algorithm in stepper_algorithms(category)) {
      tolerance <- expected_tolerance(algorithm)
      s <- make_stepper(category, algorithm)

      ## run with rodeint:
      y_r_s <- integrate_times(s, ode_r, y0, times, dt0)
      ## NOTE: different to above!
      expect_that(y_r_s, is_a("matrix"))
      expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                                check.attributes=FALSE))
      expect_that(dim(y_r_s), equals(dim(cmp)))
      expect_that(nrow(y_r_s), equals(length(times)))

      expect_that(names(attributes(y_r_s)),
                  equals(c("dim", "steps", "t", "y")))

      steps <- attr(y_r_s, "steps")
      tt    <- attr(y_r_s, "t")
      yy    <- attr(y_r_s, "y")

      expect_that(steps, is_a("numeric"))
      expect_that(tt,    is_a("numeric"))
      expect_that(yy,    is_a("numeric"))

      if (category == "dense" && algorithm == "runge_kutta_dopri5") {
        expect_that(steps, equals(length(tt) - 1))
      } else if (category == "dense" && algorithm == "bulirsch_stoer") {
        expect_that(steps, is_less_than(length(tt) - 1))
      } else {
        expect_that(steps, is_more_than(length(tt) - 1))
      }

      expect_that(tt,    is_identical_to(times))
      expect_that(yy,    equals(cmp[nrow(cmp),], tolerance=tolerance))

      ## Check the compiled version:
      expect_that(integrate_times(s, ode_cpp, y0, times, dt0),
                  is_identical_to(y_r_s))
      expect_that(integrate_times(s, ode_class, y0, times, dt0),
                  is_identical_to(y_r_s))
    }
  }
})

test_that("make_integrate", {
  pars <- 0.5
  ode_r <- ode_system(harmonic_oscillator_r, pars)
  ode_cpp <- ode_system(harmonic_oscillator_cpp, pars)
  ode_class <- ode_system(harmonic_oscillator_class, pars)

  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.05

  ## Same as the default
  s1 <- make_stepper_controlled("runge_kutta_dopri5")
  s2 <- make_stepper_basic("runge_kutta4")

  for (ode_system in list(ode_r, ode_cpp, ode_class)) {
    ## First, make my favourite thing about this -- converting f'(y, t)
    ## to f(y, t):
    f0 <- make_integrate(ode_system, t0=0, dt=dt0, save_state=FALSE)
    f1 <- make_integrate(ode_system, t0=0, dt=dt0, save_state=FALSE,
                         stepper=s1, integrate=integrate_adaptive)
    f2 <- make_integrate(ode_system, t0=0, dt=dt0, save_state=FALSE,
                         stepper=s2)
    ## And f'(y, t, pars) to f(pars)(y, t)
    g0 <- make_integrate_pars(ode_system, t0=0, dt=dt0, save_state=FALSE)
    g1 <- make_integrate_pars(ode_system, t0=0, dt=dt0, save_state=FALSE,
                              stepper=s1, integrate=integrate_adaptive)
    g2 <- make_integrate_pars(ode_system, t0=0, dt=dt0, save_state=FALSE,
                              stepper=s2)

    expect_that(names(formals(f0)), equals(c("y", "t1")))

    cmp <- integrate_adaptive(s1, ode_system, y0, t0, t1, dt0)
    expect_that(f0(y0, t1), is_identical_to(cmp))
    expect_that(f1(y0, t1), is_identical_to(cmp))
    expect_that(f2(y0, t1), not(is_identical_to(cmp)))
    expect_that(f2(y0, t1), equals(cmp, tolerance=3e-6))

    ## Check that changing parameters in underlying thing doesn't affect
    ## the generated function.
    pars2 <- pi
    ode_system$set_pars(pars2)
    cmp2 <- integrate_adaptive(s1, ode_system, y0, t0, t1, dt0)
    f3 <- make_integrate(ode_system, t0=0, dt=dt0, save_state=FALSE)
    expect_that(f0(y0, t1), is_identical_to(cmp))
    expect_that(f3(y0, t1), is_identical_to(cmp2))

    expect_that(g0(pars), is_a("function"))
    expect_that(names(formals(g0(pars))), equals(c("y", "t1")))

    expect_that(g0(pars)(y0, t1), is_identical_to(cmp))
    expect_that(g1(pars)(y0, t1), is_identical_to(cmp))
    expect_that(g2(pars)(y0, t1), not(is_identical_to(cmp)))
    expect_that(g2(pars)(y0, t1), equals(cmp, tolerance=1e-6))

    ## This is easier to use than the above (f3) when parameters change:
    expect_that(g0(pars2)(y0, t1), is_identical_to(cmp2))
  }
})
