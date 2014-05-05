source("helper-rodeint.R")

context("stepper_basic")

test_that("corner cases", {
  expect_that(stepper_basic("nonexistant"),
              throws_error())
})

test_that("construction", {
  for (type in stepper_basic_types()) {
    s <- stepper_basic(type)
    expect_that(s, is_a("stepper_basic"))

    expect_that(s$ptr, is_a("externalptr"))
    expect_that(s$ptr <- s$ptr, throws_error("read-only"))

    expect_that(s$name, equals(type))
    expect_that(s$name <- s$name, throws_error("read-only"))

    ## Test of internal method:
    expect_that(rodeint:::stepper_basic__type(s$ptr),
                is_identical_to(type))
  }
})
