source("helper-rodeint.R")

context("stepper")

test_that("corner cases", {
  expect_that(stepper("nonexistant"),
              throws_error("no default"))
  expect_that(stepper("nonexistant", "nonexistant"),
              throws_error("Unknown type"))
  expect_that(stepper("basic", "nonexistant"),
              throws_error("Unknown type"))
  expect_that(stepper("unknown", "runge_kutta_cash_karp54"),
              throws_error("Unknown type"))
})

test_that("construction", {
  for (type in stepper_types()) {
    subtypes <- if (type == "basic")
      stepper_basic_types() else stepper_controlled_types()
    for (subtype in subtypes) {
      s <- stepper(type, subtype)

      expect_that(s$ptr, is_a("externalptr"))
      expect_that(s$ptr <- s$ptr, throws_error("read-only"))

      expect_that(s$type, equals(type))
      expect_that(s$type <- s$type, throws_error("read-only"))

      expect_that(s$subtype, equals(subtype))
      expect_that(s$subtype <- s$subtype, throws_error("read-only"))

      expect_that(rodeint:::stepper__type(s$ptr),
                  is_identical_to(c(subtype, type)))
    }
  }
})
