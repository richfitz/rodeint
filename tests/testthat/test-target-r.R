source("helper-rodeint.R")

context("target_r")

## This is the same example as in odeint's harmonic_oscillator.cpp,
## with arguments in the same order as *deSolve*, not odeint.
harm.osc <- function(t, y, pars) {
  m.gam <- pars
  c(y[[2]], -y[[1]] - m.gam * y[[2]])
}

wrap.deSolve <- function(f) function(...) list(f(...))

tt <- seq(0, 20, length=101)
y0 <- c(0, 1)
pars <- 0.5
res <- lsoda(y0, tt, wrap.deSolve(harm.osc), pars)

ode <- rodeint:::target_r__ctor(harm.osc, pars)
expect_that(ode, is_a("externalptr"))

expect_that(rodeint:::target_r__derivs(ode, y0, 0.0),
            equals(harm.osc(0.0, y0, pars)))

t <- 1
cmp <- unname(lsoda(y0, c(0, t), wrap.deSolve(harm.osc), pars)[-1,-1])
expect_that(rodeint:::integrate(ode, y0, 0, t, 0.1),
            equals(cmp, tolerance=1e-5))

res <- rodeint:::integrate_observed(ode, y0, 0, t, 0.1)
expect_that(names(res), equals(c("t", "y")))
cmp <- lsoda(y0, res$t, wrap.deSolve(harm.osc), pars)
expect_that(res$y, equals(unname(cmp[,-1]), tolerance=1e-5))

## So, how do we get things like the number of steps or estimated
## error out of this?  That's going to require creating a little
## object thing, or adding attributes on later (which is cool).
types <- c("runge_kutta_cash_karp54",
           "runge_kutta_fehlberg78",
           "runge_kutta_dopri5")

cmp <- unname(lsoda(y0, c(0, t), wrap.deSolve(harm.osc), pars)[-1,-1])
for (type in types) {
  integrate_adaptive <- rodeint:::integrate_adaptive
  s <- rodeint:::controlled_stepper__ctor(type, 1e-6, 1e-6)
  y1 <- integrate_adaptive(s, ode, y0, 0, t, 0.01)
  expect_that(y1, equals(cmp, tolerance=1e-5))
  y2 <- integrate_adaptive(s, ode, y0, 0, t, 0.01, TRUE)
  expect_that(attr(y2, "steps"), is_a("numeric"))

  integrate_adaptive <- rodeint:::integrate_adaptive_observed
  y3 <- integrate_adaptive(s, ode, y0, 0, t, 0.01)
  expect_that(y3, equals(cmp, tolerance=1e-5))
  y4 <- integrate_adaptive(s, ode, y0, 0, t, 0.01, TRUE)
  expect_that(attr(y4, "steps"), is_a("numeric"))
  n <- attr(y4, "steps")
  expect_that(attr(y4, "y"), is_a("matrix"))
  tmp <- attr(y4, "y")
  expect_that(dim(tmp), equals(c(n+1, length(y0))))
  expect_that(tmp[1,], is_identical_to(y0))
  expect_that(tmp[n+1,], is_identical_to(as.numeric(y4)))
  expect_that(attr(y4, "t"), is_a("numeric"))
  tmp <- attr(y4, "t")
  expect_that(length(tmp), equals(n+1))
  expect_that(tmp[1], equals(0))
  expect_that(tmp[n+1], equals(t))
}
