## More detailed testing on the integrate_const function

source("helper-rodeint.R")

context("integrate_const")

test_that("Basic stepper, time ends at multiple of dt", {
  pars <- 0.5
  target <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  t1 <- t0 + n * dt

  category <- "basic"
  for (type in stepper_types(category)) {
    s <- make_stepper(category, type)

    y_r <- target$integrate_const(s, y0, t0, t1, dt)
    expect_that(y_r, is_a("numeric"))

    y_r_s <- target$integrate_const(s, y0, t0, t1, dt, TRUE)
    expect_that(as.numeric(y_r_s), is_identical_to(y_r))

    t_expected <- seq(t0, t1, by=dt)
    expect_that(attr(y_r_s, "t"), is_identical_to(t_expected))
    expect_that(attr(y_r_s, "steps"), equals(n))
  }
})

test_that("Basic stepper, time ends in the middle of a step", {
  pars <- 0.5
  target <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)

  y0 <- c(0, 1)
  t0 <- 1
  n <- 20L
  dt <- 0.05
  t1 <- t0 + (n + 0.33) * dt

  category <- "basic"
  for (type in stepper_types(category)) {
    s <- make_stepper(category, type)

    y_r <- target$integrate_const(s, y0, t0, t1, dt)
    expect_that(y_r, is_a("numeric"))

    ## Run the observed stepper out to t1, and then also dt *past* t1:
    y_r_s_1 <- target$integrate_const(s, y0, t0, t1,    dt, TRUE)
    y_r_s_2 <- target$integrate_const(s, y0, t0, t1+dt, dt, TRUE)

    t_expected <- seq(t0, t1, by=dt)
    expect_that(attr(y_r_s_1, "t"), is_identical_to(t_expected))
    expect_that(attr(y_r_s_1, "steps"), equals(length(t_expected) - 1))
    expect_that(attr(y_r_s_2, "steps"), equals(length(t_expected)))

    ## Now, the estimate y_r should be between the last two points:
    expect_that(y_r,
                is_in_range(last_row(attr(y_r_s_1, "y")),
                            last_row(attr(y_r_s_2, "y"))))

    ## Graphical illustration of what is going on:
    if (interactive()) {
      matplot(attr(y_r_s_2, "t"), attr(y_r_s_2, "y"), type="o",
              pch=1, cex=.5, xlab="t", ylab="y",
              main=paste(category, type, sep=" / "))
      matpoints(attr(y_r_s_1, "t"), attr(y_r_s_1, "y"), pch=19, cex=.5)
      points(rep(t1, 2), y_r, pch=4, col=1:2, cex=2)
    }
  }
})
