source("helper-rodeint.R")

context("stepper_deSolve")

test_that("basic steppers", {
  for (algorithm in stepper_basic_algorithms()) {
    s <- make_stepper_deSolve_basic(algorithm)
    expect_that(s, is_a("stepper_deSolve"))

    expect_that(s$category, equals("basic"))
    expect_that(s$category <- s$category, throws_error("read-only"))

    expect_that(s$algorithm, equals(algorithm))
    expect_that(s$algorithm <- s$algorithm, throws_error("read-only"))

    expect_that(s$abs_tol, equals(1.0)) # NOTE: might change
    expect_that(s$abs_tol <- s$abs_tol, throws_error("read-only"))

    expect_that(s$rel_tol, equals(1.0)) # NOTE: might change
    expect_that(s$rel_tol <- s$abs_tol, throws_error("read-only"))

    expect_that(s$method, is_a("list"))
    expect_that(s$method, is_a("rkMethod"))
    expect_that(s$method <- s$method, throws_error("read-only"))
  }
})

test_that("controlled steppers", {
  for (algorithm in stepper_controlled_algorithms()) {
    s <- make_stepper_deSolve_controlled(algorithm)
    expect_that(s, is_a("stepper_deSolve"))

    expect_that(s$category, equals("controlled"))
    expect_that(s$category <- s$category, throws_error("read-only"))

    expect_that(s$algorithm, equals(algorithm))
    expect_that(s$algorithm <- s$algorithm, throws_error("read-only"))

    expect_that(s$abs_tol, equals(1e-6))
    expect_that(s$abs_tol <- s$abs_tol, throws_error("read-only"))

    expect_that(s$rel_tol, equals(1e-6))
    expect_that(s$rel_tol <- s$abs_tol, throws_error("read-only"))

    expect_that(s$method, is_a("list"))
    expect_that(s$method, is_a("rkMethod"))
    expect_that(s$method <- s$method, throws_error("read-only"))

    ## Test setting abs_tol/rel_tol
    abs_tol <- 1e-10
    rel_tol <- 1e-04
    s <- make_stepper_deSolve_controlled(algorithm, abs_tol, rel_tol)
    expect_that(s$abs_tol, equals(abs_tol))
    expect_that(s$rel_tol, equals(rel_tol))
  }
})
