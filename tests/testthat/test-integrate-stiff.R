source("helper-rodeint.R")

context("integrate (stiff)")

expected_tolerance <- function(algorithm) {
  switch(algorithm,
         euler=0.03, # such inaccuracy
         modified_midpoint=1e-4,
         3e-5)
}

test_that("integate_adaptive", {
  pars <- numeric(0)
  ode_r <- ode_system(stiff_r_derivs, pars,
                      jacobian=stiff_r_jacobian)
  ode_cpp <- ode_system(stiff_cpp, pars)
  ode_class <- ode_system(stiff_class, pars)

  set.seed(1)
  y0 <- runif(2)
  t0 <- 0
  t1 <- 4
  dt0 <- 0.01

  cmp <- unname(lsoda(y0, c(t0, t1), func=stiff_r_derivs_deSolve,
                      jacfunc=stiff_r_jacobian_deSolve,
                      atol=1e-8, rtol=1e-8)[-1,-1])

  for (category in stepper_categories()) {
    for (algorithm in stepper_algorithms(category, have_jacobian=TRUE)) {
      tolerance <- expected_tolerance(algorithm)
      s <- make_stepper(category, algorithm, ublas_state=TRUE)

      ## TODO: Here, and in test-integrate.R, y_r -> y
      y_r <- integrate_adaptive(s, ode_r, y0, t0, t1, dt0)
      expect_that(y_r, is_a("numeric"))
      expect_that(y_r, equals(cmp, tolerance=tolerance))

      y_r_s <- integrate_adaptive(s, ode_r, y0, t0, t1, dt0, TRUE)
      expect_that(y_r_s, equals(cmp, tolerance=tolerance,
                                check.attributes=FALSE))

      expect_that(names(attributes(y_r_s)),
                  equals(c("steps", "t", "y")))

      steps <- attr(y_r_s, "steps")
      tt    <- attr(y_r_s, "t")
      yy    <- attr(y_r_s, "y")

      expect_that(steps, is_a("numeric"))
      expect_that(tt,    is_a("numeric"))
      expect_that(yy,    is_a("matrix"))

      expect_that(tt[[1]],         is_identical_to(t0))
      expect_that(tt[[steps + 1]], is_identical_to(t1))

      expect_that(yy[1,],         is_identical_to(y0))
      expect_that(yy[steps + 1,], is_identical_to(as.numeric(y_r_s)))

      ## Check the compiled version:
      expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_adaptive(s, ode_cpp, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      expect_that(integrate_adaptive(s, ode_class, y0, t0, t1, dt0),
                  is_identical_to(y_r))
      expect_that(integrate_adaptive(s, ode_class, y0, t0, t1, dt0, TRUE),
                  is_identical_to(y_r_s))

      ## Check that we still can integrate with a nonstiff stepper.
      ##
      ## This is doing a conversion for us behind the scenes.
      if (algorithm != "rosenbrock4") {
        s_nonstiff <- make_stepper(category, algorithm, ublas_state=FALSE)
        expect_that(integrate_adaptive(s_nonstiff, ode_r, y0, t0, t1, dt0),
                    is_identical_to(y_r))
        expect_that(integrate_adaptive(s_nonstiff, ode_cpp, y0, t0, t1, dt0),
                    is_identical_to(y_r))
        expect_that(integrate_adaptive(s_nonstiff, ode_class, y0, t0, t1, dt0),
                    is_identical_to(y_r))
        ## Pass-by-value leaves stepper unchanged.
        expect_that(attr(s_nonstiff$details(), "ublas_state"), is_false())
      }
    }
  }
})
