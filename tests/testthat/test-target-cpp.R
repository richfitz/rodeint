source("helper-rodeint.R")

context("target_cpp")

test_that("construction", {
  pars <- 0.5
  obj <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)
  expect_that(obj, is_a("target_cpp"))
  expect_that(obj$ptr <- obj$ptr,
              throws_error("read-only"))
})

test_that("derivatives", {
  pars <- 0.5
  obj <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)
  y0 <- c(0, 1)
  t0 <- 0.0
  expect_that(obj$derivs(y0, t0),
              is_identical_to(harmonic.oscillator(t0, y0, pars)))
})

test_that("parameters", {
  pars <- 0.5
  obj <- target_cpp(rodeint:::test_harmonic_oscillator_cpp, pars)
  expect_that(obj$pars(), is_identical_to(pars))
  ## In contrast with target_r, this will throw:
  pars2 <- list(a=1, b=2)
  expect_that(obj$set_pars(pars2), throws_error("not compatible"))
  ## This should also throw but seems not to...
  expect_that(obj$set_pars(rep(pars, 2)), throws_error())
  expect_that(obj$set_pars(numeric(0)),   throws_error())
})


## Disabled for now, because sourceCpp seems poorly behaved during R
## CMD check.  Not sure why...

## context("target_cpp [via sourceCpp, compilation may be slow]")

## test_that("via sourceCpp", {
##   ## TODO: For some reason, if I do sourceCpp(file.cpp), then it fails
##   ## because it can't find BH, if the BH version is set.  We *do*
##   ## depend on (>= 1.54.0-1), and that should follow BH.  For now I've
##   ## stripped the version dependency, but this looks like a Rcpp bug.
##   file.cpp <- system.file("examples/harmonic_oscillator.cpp",
##                           package="rodeint")
##   Rcpp::sourceCpp(code=paste(readLines(file.cpp), collapse="\n"))
##   pars <- 0.5
##   obj <- target_cpp(example_harmonic_oscillator, pars)
##   y0 <- c(0, 1)
##   t0 <- 0.0
##   expect_that(obj$derivs(y0, t0),
##               is_identical_to(harmonic.oscillator(t0, y0, pars)))
## })
