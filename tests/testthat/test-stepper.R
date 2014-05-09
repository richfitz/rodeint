source("helper-rodeint.R")

context("stepper")

test_that("stepper lists", {
  expect_that(stepper_categories(), equals(c("basic", "controlled")))

  expect_that(stepper_basic_types(),
              equals(c("euler", "modified_midpoint", "runge_kutta4",
                       "runge_kutta_cash_karp54",
                       "runge_kutta_fehlberg78",
                       "runge_kutta_dopri5")))
  expect_that(stepper_controlled_types(),
              equals(c("runge_kutta_cash_karp54",
                       "runge_kutta_fehlberg78",
                       "runge_kutta_dopri5")))

  expect_that(stepper_types("basic"),
              is_identical_to(stepper_basic_types()))
  expect_that(stepper_types("controlled"),
              is_identical_to(stepper_controlled_types()))

  expect_that(stepper_types("nonexistant"),
              throws_error("Invalid stepper category"))
})

test_that("corner cases", {
  expect_that(make_stepper_basic("nonexistant"),
              throws_error("Unknown type"))
  expect_that(make_stepper_controlled("nonexistant"),
              throws_error("Unknown type"))
  ## Controlled steppers take tolerance arguments that must be
  ## numeric:
  expect_that(make_stepper_controlled("runge_kutta_cash_karp54", "a"),
              throws_error("numeric"))
})

test_that("basic steppers", {
  for (type in stepper_basic_types()) {
    s <- make_stepper_basic(type)
    expect_that(s, is_a("stepper"))

    expect_that(s$ptr, is_a("externalptr"))
    expect_that(s$ptr <- s$ptr, throws_error("read-only"))

    expect_that(s$category, equals("basic"))
    expect_that(s$category <- s$category, throws_error("read-only"))

    expect_that(s$type, equals(type))
    expect_that(s$type <- s$type, throws_error("read-only"))

    expect_that(s$atol, equals(NA_real_))
    expect_that(s$atol <- s$atol, throws_error("read-only"))

    expect_that(s$rtol, equals(NA_real_))
    expect_that(s$rtol <- s$atol, throws_error("read-only"))

    ## Test of internal method:
    expect_that(rodeint:::stepper__type(s$ptr),
                is_identical_to(c("basic", type)))
  }
})

test_that("controlled steppers", {
  for (type in stepper_controlled_types()) {
    s <- make_stepper_controlled(type)
    expect_that(s, is_a("stepper"))

    expect_that(s$category, equals("controlled"))
    expect_that(s$category <- s$category, throws_error("read-only"))

    expect_that(s$ptr, is_a("externalptr"))
    expect_that(s$ptr <- s$ptr, throws_error("read-only"))

    expect_that(s$type, equals(type))
    expect_that(s$type <- s$type, throws_error("read-only"))

    expect_that(s$atol, equals(1e-6))
    expect_that(s$atol <- s$atol, throws_error("read-only"))

    expect_that(s$rtol, equals(1e-6))
    expect_that(s$rtol <- s$atol, throws_error("read-only"))

    ## Test of internal method:
    expect_that(rodeint:::stepper__type(s$ptr),
                is_identical_to(c("controlled", type)))

    ## Test setting atol/rtol
    atol <- 1e-10
    rtol <- 1e-04
    s <- make_stepper_controlled(type, atol, rtol)
    expect_that(s$atol, equals(atol))
    expect_that(s$rtol, equals(rtol))
  }
})
