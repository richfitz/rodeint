library(deSolve)
library(rodeint)

## This is the same example as in odeint's harmonic_oscillator.cpp:
harm.osc <- function(t, y, pars) {
  m.gam <- pars
  c(y[[2]], -y[[1]] - m.gam * y[[2]])
}

wrap.deSolve <- function(f) function(...) list(f(...))

tt <- seq(0, 20, length=101)
y0 <- c(0, 1)
pars <- 0.5
res <- lsoda(y0, tt, wrap.deSolve(harm.osc), pars)


t <- 1
y <- rodeint:::harmonic_oscillator_basic(y0, 0, t, pars, 0.1)
matplot(res[,1], res[,2:3], type="l")
points(rep(t, length(y)), y, col=1:2)

# Now, try and pass this into Rcpp:

ode <- rodeint:::ode_target_r_make(harm.osc, pars)

rodeint:::ode_target_r_set_pars(ode, pars)
rodeint:::ode_target_r_derivs(ode, y0, 0.0)
rodeint:::ode_target_r_basic(ode, y0, 0, t, 0.1)

int <- rodeint:::integrator_make(ode)
rodeint:::integrator_derivs(int, y0, 0.0)

rodeint:::integrator_integrate(int, y0, 0, t, 0.1)

## Here's the Rcpp overhead -- it't not actually that pretty.  There
## is a 3% x2 penalty for RNG handling, but the rest is due to the
## fact that calling R in this way seems less efficient than native.
## Perhaps not that surprising!
##
## There are also several full copies that R gets to avoid going on,
## and those could probably be dealt with more neatly.
library(microbenchmark)
microbenchmark(harm.osc(0, y0, pars),
               rodeint:::ode_target_r_derivs(ode, y0, 0.0),
               rodeint:::integrator_derivs(int, y0, 0.0),
               times=1000L)

## These are basically the same amount of time, which is probably a
## good sign.  If anything, passing in the ode is as fast as building
## an integrator, which I like.
microbenchmark(rodeint:::ode_target_r_basic(ode, y0, 0, t, 0.1),
               rodeint:::integrator_integrate(int, y0, 0, t, 0.1))

## The big potential issue is going to be swapping out different
## integration backends fairly seamlessly.  Doing this *within* C++
## code is a touch easier because I can natively use templates.

## How would this all work anyway?  Most of the time, from R, we'd
## probably be happy passing in a function, or a reference to a
## compiled function, and specifying some options, I think.

## What would be nice would be to have a way of orthoganlising things
## - especially monitoring the calls.
