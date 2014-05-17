source("helper-rodeint.R")

context("stepper")

test_that("stepper lists", {
  expect_that(stepper_categories(), equals(c("basic", "controlled")))
  expect_that(stepper_basic_algorithms(),
              equals(c("euler", "modified_midpoint", "runge_kutta4",
                       "runge_kutta_cash_karp54",
                       "runge_kutta_fehlberg78",
                       "runge_kutta_dopri5")))
  expect_that(stepper_controlled_algorithms(),
              equals(c("runge_kutta_cash_karp54",
                       "runge_kutta_fehlberg78",
                       "runge_kutta_dopri5",
                       "bulirsch_stoer")))

  expect_that(stepper_basic_algorithms(have_jacobian=TRUE),
              equals(c(stepper_basic_algorithms(), "rosenbrock4")))
  expect_that(stepper_controlled_algorithms(have_jacobian=TRUE),
              equals(c(stepper_controlled_algorithms(), "rosenbrock4")))

  expect_that(stepper_algorithms("basic"),
              is_identical_to(stepper_basic_algorithms()))
  expect_that(stepper_algorithms("controlled"),
              is_identical_to(stepper_controlled_algorithms()))
  expect_that(stepper_algorithms("basic", TRUE),
              is_identical_to(stepper_basic_algorithms(TRUE)))
  expect_that(stepper_algorithms("controlled", TRUE),
              is_identical_to(stepper_controlled_algorithms(TRUE)))

  expect_that(stepper_algorithms("nonexistant"),
              throws_error("Invalid stepper category"))
})

test_that("corner cases", {
  expect_that(make_stepper("nonexistant", "euler"),
              throws_error("Invalid stepper category"))
  expect_that(make_stepper_basic("nonexistant"),
              throws_error("Invalid algorithm"))
  expect_that(make_stepper_controlled("nonexistant"),
              throws_error("Invalid algorithm"))

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
    for (algorithm in stepper_basic_algorithms()) {
      if ((category == "basic"      &&
           algorithm %in% stepper_basic_algorithms()) ||
          (category == "controlled" &&
           algorithm %in% stepper_controlled_algorithms())) {
        s <- make_stepper(category, algorithm)
        expect_that(s, is_a("stepper"))

        expect_that(s$ptr, is_a("externalptr"))
        expect_that(s$category, equals(category))
        expect_that(s$algorithm, equals(algorithm))
        expect_that(s$abs_tol, equals(tol_default[[category]][[1]]))
        expect_that(s$rel_tol, equals(tol_default[[category]][[2]]))

        ## All fields are read only (except ptr)
        expect_that(s$category <- s$category, throws_error("read-only"))
        expect_that(s$algorithm <- s$algorithm, throws_error("read-only"))
        expect_that(s$abs_tol <- s$abs_tol, throws_error("read-only"))
        expect_that(s$rel_tol <- s$abs_tol, throws_error("read-only"))

        ## Test of internal method & some internal consistency.
        ## Includes attributes.
        category_id  <- match(category,  stepper_categories()) - 1L
        algorithm_id <- match(algorithm, stepper_basic_algorithms()) - 1L
        if (algorithm == "bulirsch_stoer") {
          algorithm_id <- length(stepper_basic_algorithms())
        }

        cmp <- structure(c(category, algorithm),
                         category_id=category_id,
                         algorithm_id=algorithm_id,
                         ublas_state=FALSE, needs_jacobian=FALSE)
        expect_that(s$details(), is_identical_to(cmp))

        ## Print method:
        expect_that(s$show(), prints_text("stepper for solving"))
        expect_that(s$show(), not(prints_text("addr:")))
        expect_that(s$show(TRUE), prints_text("addr:"))
      } else {
        expect_that(make_stepper(category, algorithm),
                    throws_error("Cannot make a"))
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
    for (algorithm in stepper_basic_algorithms()) {
      if ((category == "basic"      &&
           algorithm %in% stepper_basic_algorithms()) ||
          (category == "controlled" &&
           algorithm %in% stepper_controlled_algorithms())) {
        ## Set tolerances and see if they stick
        if (category == "basic") {
          expect_that(s <- make_stepper(category, algorithm,
                                        tol_given[[1]], tol_given[[2]]),
                      gives_warning("Ignoring provided tolerance"))
          expect_that(s$abs_tol, equals(tol_result[[category]][[1]]))
          expect_that(s$rel_tol, equals(tol_result[[category]][[2]]))
        } else {
          s <- make_stepper(category, algorithm,
                            tol_given[[1]], tol_given[[2]])
          expect_that(s$abs_tol, equals(tol_result[[category]][[1]]))
          expect_that(s$rel_tol, equals(tol_result[[category]][[2]]))

          expect_that(make_stepper(category, algorithm,
                                   NA_real_, tol_given[[2]]),
                      throws_error("Tolerances must be non-NA"))
          expect_that(make_stepper(category, algorithm,
                                   tol_given[[1]], NA_real_),
                      throws_error("Tolerances must be non-NA"))
          expect_that(make_stepper(category, algorithm,
                                   NA_real_, NA_real_),
                      throws_error("Tolerances must be non-NA"))
        }
      }
    }
  }
})

test_that("stiff steppers (really implicit)", {
  for (category in stepper_categories()) {
    for (algorithm in stepper_basic_algorithms(TRUE)) {
      if ((category == "basic"      &&
           algorithm %in% stepper_basic_algorithms(TRUE)) ||
          (category == "controlled" &&
           algorithm %in% stepper_controlled_algorithms(TRUE))) {
        s <- make_stepper(category, algorithm, ublas_state=TRUE)
        expect_that(s, is_a("stepper"))
        category_id <- match(category, stepper_categories()) - 1L
        if (algorithm == "rosenbrock4") {
          algorithm_id <- length(stepper_basic_algorithms()) + 1L
          cmp <- structure(c(category, algorithm),
                           category_id=category_id,
                           algorithm_id=algorithm_id,
                           ublas_state=TRUE, needs_jacobian=TRUE)
        } else {
          algorithm_id <- match(algorithm, stepper_basic_algorithms()) - 1L
          if (algorithm == "bulirsch_stoer") {
            algorithm_id <- length(stepper_basic_algorithms())
          }

          cmp <- structure(c(category, algorithm),
                           category_id=category_id,
                           algorithm_id=algorithm_id,
                           ublas_state=TRUE, needs_jacobian=FALSE)
        }
        expect_that(s$details(), is_identical_to(cmp))
      } else {
        expect_that(make_stepper(category, algorithm),
                    throws_error("Cannot make a"))
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

test_that("serialisation", {
  s <- make_stepper("basic", "euler")
  f <- tempfile(fileext=".rds")
  saveRDS(s, f)
  restored <- readRDS(f)
  expect_that(rodeint:::ptr_valid(restored$ptr), is_false())
  expect_that(restored$details(), throws_error(NULL))
  restored$rebuild()
  expect_that(rodeint:::ptr_valid(restored$ptr), is_true())
  expect_that(restored$details(), is_identical_to(s$details()))
})
