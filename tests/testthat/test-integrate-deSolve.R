## Integration using deSolve steppers
source("helper-rodeint.R")

context("integrate (using deSolve)")

## Proof of concept only:
test_that("integrate_adaptive", {
  pars <- 0.5
  obj <- ode_system(harmonic_oscillator_r, pars)
  y0 <- c(0, 1)
  t0 <- 0
  t1 <- 1
  dt0 <- 0.01

  category <- "controlled"
  for (algorithm in stepper_algorithms(category)) {
    if (algorithm == "bulirsch_stoer") {
      expect_that(make_stepper_deSolve(category, algorithm),
                  throws_error("Unknown algorithm"))
      next
    }
    stepper <- make_stepper_deSolve(category, algorithm)


    cmp <- unname(lsoda(y0, c(t0, t1), harmonic_oscillator_deSolve,
                        pars)[-1,-1])

    y1 <- rodeint:::integrate_adaptive_deSolve(stepper, obj, y0,
                                               t0, t1, dt0)
    expect_that(y1, equals(cmp, tolerance=1e-5))

    expect_that(integrate_adaptive(stepper, obj, y0, t0, t1, dt0),
                is_identical_to(y1))

    s <- make_stepper("controlled", algorithm)
    y2 <- integrate_adaptive(s, obj, y0, t0, t1, dt0)
    expect_that(y2, equals(y1, tolerance=1e-6))
    expect_that(y2, not(is_identical_to(y1)))

    expect_that(integrate_adaptive(stepper, obj, y0, t0, t1, dt0, TRUE),
                throws_error("not supported"))
  }
})
