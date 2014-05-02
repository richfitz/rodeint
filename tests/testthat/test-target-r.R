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
