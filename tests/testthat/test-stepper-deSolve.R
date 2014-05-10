source("helper-rodeint.R")

context("stepper_deSolve")

test_that("basic steppers", {
  for (type in stepper_basic_types()) {
    s <- make_stepper_deSolve_basic(type)
    expect_that(s, is_a("stepper_deSolve"))

    expect_that(s$category, equals("basic"))
    expect_that(s$category <- s$category, throws_error("read-only"))

    expect_that(s$type, equals(type))
    expect_that(s$type <- s$type, throws_error("read-only"))

    expect_that(s$atol, equals(1.0)) # NOTE: might change
    expect_that(s$atol <- s$atol, throws_error("read-only"))

    expect_that(s$rtol, equals(1.0)) # NOTE: might change
    expect_that(s$rtol <- s$atol, throws_error("read-only"))

    expect_that(s$method, is_a("list"))
    expect_that(s$method, is_a("rkMethod"))
    expect_that(s$method <- s$method, throws_error("read-only"))
  }
})

test_that("controlled steppers", {
  for (type in stepper_controlled_types()) {
    s <- make_stepper_deSolve_controlled(type)
    expect_that(s, is_a("stepper_deSolve"))

    expect_that(s$category, equals("controlled"))
    expect_that(s$category <- s$category, throws_error("read-only"))

    expect_that(s$type, equals(type))
    expect_that(s$type <- s$type, throws_error("read-only"))

    expect_that(s$atol, equals(1e-6))
    expect_that(s$atol <- s$atol, throws_error("read-only"))

    expect_that(s$rtol, equals(1e-6))
    expect_that(s$rtol <- s$atol, throws_error("read-only"))

    expect_that(s$method, is_a("list"))
    expect_that(s$method, is_a("rkMethod"))
    expect_that(s$method <- s$method, throws_error("read-only"))

    ## Test setting atol/rtol
    atol <- 1e-10
    rtol <- 1e-04
    s <- make_stepper_deSolve_controlled(type, atol, rtol)
    expect_that(s$atol, equals(atol))
    expect_that(s$rtol, equals(rtol))
  }
})