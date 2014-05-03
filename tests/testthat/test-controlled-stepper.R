source("helper-rodeint.R")

context("controlled_stepper")

test_that("corner cases", {
  expect_that(controlled_stepper("nonexistant"),
              throws_error())
})

test_that("construction", {
  types <- c("runge_kutta_cash_karp54",
             "runge_kutta_fehlberg78",
             "runge_kutta_dopri5")
  for (type in types) {
    s <- controlled_stepper(type)
    expect_that(s, is_a("controlled_stepper"))

    expect_that(s$ptr, is_a("externalptr"))
    expect_that(s$ptr <- s$ptr, throws_error("read-only"))

    expect_that(s$name, equals(type))
    expect_that(s$name <- s$name, throws_error("read-only"))

    expect_that(s$atol, equals(1e-6))
    expect_that(s$atol <- s$atol, throws_error("read-only"))

    expect_that(s$rtol, equals(1e-6))
    expect_that(s$rtol <- s$atol, throws_error("read-only"))

    ## Test of internal method:
    expect_that(rodeint:::controlled_stepper__type(s$ptr),
                is_identical_to(type))

    ## Test setting atol/rtol
    atol <- 1e-10
    rtol <- 1e04
    s <- controlled_stepper(type, atol, rtol)
    expect_that(s$atol, equals(atol))
    expect_that(s$rtol, equals(rtol))
  }
})
