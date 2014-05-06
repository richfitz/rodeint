source("helper-rodeint.R")

context("stepper_controlled")

test_that("corner cases", {
  expect_that(make_stepper_basic("nonexistant"),
              throws_error())
  expect_that(make_stepper_controlled("nonexistant"),
              throws_error())
  expect_that(make_stepper_controlled("runge_kutta_cash_karp54", "a"),
              throws_error())
})

test_that("basic steppers", {
  for (type in stepper_basic_types()) {
    type <- "runge_kutta4"
    s <- rodeint:::make_stepper_basic(type)

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
    expect_that(rodeint:::stepper_controlled__type(s$ptr),
                is_identical_to(type))
  }
})

test_that("construction", {
  for (type in stepper_controlled_types()) {
    s <- make_stepper_controlled(type)
    expect_that(s, is_a("stepper_controlled"))

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
    expect_that(rodeint:::stepper_controlled__type(s$ptr),
                is_identical_to(type))

    ## Test setting atol/rtol
    atol <- 1e-10
    rtol <- 1e-04
    s <- make_stepper_controlled(type, atol, rtol)
    expect_that(s$atol, equals(atol))
    expect_that(s$rtol, equals(rtol))
  }
})
