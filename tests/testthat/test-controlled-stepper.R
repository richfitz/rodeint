source("helper-rodeint.R")

context("controlled_stepper")

test_that("corner cases", {
  expect_that(rodeint:::controlled_stepper__ctor("nonexistant", 1e-6, 1e-6),
              throws_error())
  expect_that(rodeint:::controlled_stepper__ctor("nonexistant"),
              throws_error())
  expect_that(rodeint:::controlled_stepper__ctor("nonexistant", 1e-6),
              throws_error())
})

test_that("construction", {
  types <- c("runge_kutta_cash_karp54",
             "runge_kutta_fehlberg78",
             "runge_kutta_dopri5")
  for (t in types) {
    s <- rodeint:::controlled_stepper__ctor(t, 1e-6, 1e-6)
    expect_that(s, is_a("externalptr"))
    expect_that(rodeint:::controlled_stepper__type(s),
                is_identical_to(t))
  }
})
