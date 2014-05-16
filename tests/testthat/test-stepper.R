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

  expect_that(stepper_basic_types(have_jacobian=TRUE),
              equals(c(stepper_basic_types(), "rosenbrock4")))
  expect_that(stepper_controlled_types(have_jacobian=TRUE),
              equals(c(stepper_controlled_types(), "rosenbrock4")))

  expect_that(stepper_types("basic"),
              is_identical_to(stepper_basic_types()))
  expect_that(stepper_types("controlled"),
              is_identical_to(stepper_controlled_types()))
  expect_that(stepper_types("basic", TRUE),
              is_identical_to(stepper_basic_types(TRUE)))
  expect_that(stepper_types("controlled", TRUE),
              is_identical_to(stepper_controlled_types(TRUE)))

  expect_that(stepper_types("nonexistant"),
              throws_error("Invalid stepper category"))
})

## TODO: error messages are inconsistent.
test_that("corner cases", {
  expect_that(make_stepper("nonexistant", "euler"),
              throws_error("Invalid stepper category"))
  expect_that(make_stepper_basic("nonexistant"),
              throws_error("Invalid type"))
  expect_that(make_stepper_controlled("nonexistant"),
              throws_error("Invalid type"))

  ## TODO: Expand this out and check nonscalar, negative.
  ## Controlled steppers take tolerance arguments that must be
  ## numeric:
  expect_that(make_stepper_controlled("runge_kutta_cash_karp54", "a"),
              throws_error("numeric"))
})

test_that("construction", {
  ## What is set by default:
  tol_default <- list(basic=rep(NA_real_, 2),
                      controlled=rep(1e-6, 2))
  for (category in stepper_categories()) {
    for (type in stepper_basic_types()) {
      if ((category == "basic"      && type %in% stepper_basic_types()) ||
          (category == "controlled" && type %in% stepper_controlled_types())) {
        s <- make_stepper(category, type)
        expect_that(s, is_a("stepper"))

        expect_that(s$ptr, is_a("externalptr"))
        expect_that(s$category, equals(category))
        expect_that(s$type, equals(type))
        expect_that(s$abs_tol, equals(tol_default[[category]][[1]]))
        expect_that(s$rel_tol, equals(tol_default[[category]][[2]]))

        ## All fields are read only:
        expect_that(s$ptr <- s$ptr, throws_error("read-only"))
        expect_that(s$category <- s$category, throws_error("read-only"))
        expect_that(s$type <- s$type, throws_error("read-only"))
        expect_that(s$abs_tol <- s$abs_tol, throws_error("read-only"))
        expect_that(s$rel_tol <- s$abs_tol, throws_error("read-only"))

        ## Test of internal method & some internal consistency.
        ## Includes attributes.
        cmp <- structure(c(category, type),
                         category_id=match(category, stepper_categories()) - 1L,
                         type_id=match(type, stepper_basic_types()) - 1L,
                         ublas_state=FALSE, needs_jacobian=FALSE)
        expect_that(s$details(), is_identical_to(cmp))
      } else {
        expect_that(make_stepper(category, type),
                    throws_error("Cannot make a controlled stepper"))
      }
    }
  }
})

test_that("tolerance", {
  ## What we will provide as a test case:
  tol_given   <- c(1e-8, 1e-9)
  ## What we expect back:
  tol_result <- list(basic=rep(NA_real_, 2),
                     controlled=tol_given)

  for (category in stepper_categories()) {
    for (type in stepper_basic_types()) {
      if ((category == "basic"      && type %in% stepper_basic_types()) ||
          (category == "controlled" && type %in% stepper_controlled_types())) {
        ## Set tolerances and see if they stick
        if (category == "basic") {
          expect_that(s <- make_stepper(category, type,
                                        tol_given[[1]], tol_given[[2]]),
                      gives_warning("Ignoring provided tolerance"))
          expect_that(s$abs_tol, equals(tol_result[[category]][[1]]))
          expect_that(s$rel_tol, equals(tol_result[[category]][[2]]))
        } else {
          s <- make_stepper(category, type, tol_given[[1]], tol_given[[2]])
          expect_that(s$abs_tol, equals(tol_result[[category]][[1]]))
          expect_that(s$rel_tol, equals(tol_result[[category]][[2]]))

          expect_that(make_stepper(category, type, NA_real_, tol_given[[2]]),
                      throws_error("Tolerances must be non-NA"))
          expect_that(make_stepper(category, type, tol_given[[1]], NA_real_),
                      throws_error("Tolerances must be non-NA"))
          expect_that(make_stepper(category, type, NA_real_, NA_real_),
                      throws_error("Tolerances must be non-NA"))
        }
      }
    }
  }
})

test_that("stiff steppers (really implicit)", {
  for (category in stepper_categories()) {
    for (type in stepper_basic_types(TRUE)) {
      if ((category == "basic"      &&
           type %in% stepper_basic_types(TRUE)) ||
          (category == "controlled" &&
           type %in% stepper_controlled_types(TRUE))) {
        s <- make_stepper(category, type, ublas_state=TRUE)
        expect_that(s, is_a("stepper"))
        category_id <- match(category, stepper_categories()) - 1L
        cmp <- structure(c(category, type),
                         category_id=category_id,
                         type_id=length(stepper_basic_types())+1L,
                         ublas_state=TRUE, needs_jacobian=TRUE)
        if (type == "rosenbrock4") {
        } else {
          cmp <- structure(c(category, type),
                           category_id=category_id,
                           type_id=match(type, stepper_basic_types()) - 1L,
                           ublas_state=TRUE, needs_jacobian=FALSE)
        }
        expect_that(s$details(), is_identical_to(cmp))
      } else {
        expect_that(make_stepper(category, type),
                    throws_error("Cannot make a controlled stepper"))
      }
    }
  }
})

test_that("Can't make stiff stepper with nonstiff state", {
  for (category in stepper_categories()) {
    expect_that(make_stepper(category, "rosenbrock4", ublas_state=FALSE),
                throws_error("requires a uBLAS state"))
  }
})
