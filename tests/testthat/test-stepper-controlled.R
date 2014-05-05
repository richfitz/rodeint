source("helper-rodeint.R")

context("stepper_controlled")

test_that("corner cases", {
  expect_that(stepper_controlled("nonexistant"),
              throws_error())
})

test_that("construction", {
  for (type in stepper_controlled_types()) {
    s <- stepper_controlled(type)
    expect_that(s, is_a("stepper_controlled"))

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
    rtol <- 1e04
    s <- stepper_controlled(type, atol, rtol)
    expect_that(s$atol, equals(atol))
    expect_that(s$rtol, equals(rtol))
  }
})
