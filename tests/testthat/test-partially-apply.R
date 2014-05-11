source("helper-rodeint.R")

context("partially_apply")

fnames <- function(f) {
  names(formals(f))
}

test_that("partially_apply", {
  ## Here is a stupid function, but it should serve as a reasonable
  ## test case.
  target <- function(a, b="x", ..., c) {
    list(a=a, b=b, c=c, dots=list(...))
  }

  ## This is a cheat because really nothing happens in the function.
  ## But that might change down the road.
  expect_that(partially_apply(target), is_identical_to(target))

  ## Set a single argument:
  aval <- pi
  bval <- list()
  cval <- 1:10

  f1 <- partially_apply(target, a=aval, set_as_defaults=TRUE)
  f2 <- partially_apply(target, a=aval, set_as_defaults=FALSE)
  cmp <- target(aval, c=cval, x=1)

  expect_that(f1(c=cval, x=1), is_identical_to(cmp))
  expect_that(f2(c=cval, x=1), is_identical_to(cmp))

  args <- setdiff(fnames(target), "a")
  expect_that(fnames(f1), equals(c(args, "a")))
  expect_that(fnames(f2), equals(args))

  ## Set all formals:
  f1 <- partially_apply(target, c=cval, a=aval, b=bval, set_as_defaults=TRUE)
  f2 <- partially_apply(target, c=cval, a=aval, b=bval, set_as_defaults=FALSE)

  ## Order of arguments same as above.
  expect_that(fnames(f1), equals(c("...", "c", "a", "b")))
  expect_that(fnames(f2), equals("..."))

  cmp <- target(c=cval, a=aval, b=bval)
  expect_that(f1(), is_identical_to(cmp))
  expect_that(f2(), is_identical_to(cmp))

  cmp <- target(c=cval, a=aval, b=bval, x=1)
  expect_that(f1(x=1), is_identical_to(cmp))
  expect_that(f2(x=1), is_identical_to(cmp))

  expect_that(partially_apply(is.integer, x=1),
              throws_error("Cannot partially apply primitive functions"))

  ## This does follow from the first rule, but prevents regressions.
  target0 <- function() {
    "constant function"
  }
  expect_that(partially_apply(target0), is_identical_to(target0))

  ## Nonexistant args:
  expect_that(partially_apply(target, d=1),
              throws_error("Unknown arguments"))
  expect_that(partially_apply(target, d=1, e=2),
              throws_error("Unknown arguments"))
  expect_that(partially_apply(target, a=1, e=2),
              throws_error("Unknown arguments"))

  ## Duplicated arg:
  expect_that(partially_apply(target, a=1, a=2),
              throws_error("Duplicated formal arguments"))

  ## Requires that supplied values are evaluatable:
  expect_that(partially_apply(target, d=does_not_exist),
              throws_error("not found"))

  ## Unnamed arguments
  expect_that(partially_apply(target, 1),
              throws_error("must be named"))
  expect_that(partially_apply(target, a=1, 1),
              throws_error("must be named"))

  ## Check that the environment tricks aren't too tricky:
  target1 <- function(arg) {
    arg
  }

  z <- 1
  fe <- partially_apply(target1, arg=z)
  fd <- partially_apply(target1, arg=z, set_as_defaults=TRUE)

  expect_that(ls(environment(fe)), equals("arg"))
  expect_that(environment(fe)$arg, equals(z))

  ## Ideally I could just check that it's pointing at the same
  ## environment, but can't work out how to do that!
  expect_that(ls(environment(fd)),
              equals(ls(environment(target))))

  ## Both works after deletion -- not seeing *this* z:
  rm(z)
  expect_that(fe(), equals(1))
  expect_that(fd(), equals(1))

  ## Add some attributes to a function:
  obj <- list(class="foo", info=1:10)
  tmp <- target
  for (i in names(obj)) {
    attr(tmp, i) <- obj[[i]]
  }

  z <- 1
  tmp_e <- partially_apply(tmp, a=z)
  tmp_d <- partially_apply(tmp, a=z, set_as_defaults=TRUE)

  expect_that(attributes(tmp_e), is_identical_to(obj))
  expect_that(attributes(tmp_d), is_identical_to(obj))

  ## Check still find global/local/lexical things.
  assign("x_global", 1, .GlobalEnv)
  x_testthat <- 2
  foo <- local({
    x_local <- 3
    function(x_lexical, y_lexical) {
      function(x_arg, y_arg, z_arg) {
        list(x_global, x_testthat, x_local, x_lexical, y_lexical,
             x_arg, y_arg, z_arg)
      }
    }
  })
  scope <- foo(4, 5)
  cmp <- as.list(1:8)

  scope_e <- partially_apply(scope, x_arg=6, y_arg=7, z_arg=8)
  scope_d <- partially_apply(scope, x_arg=6, y_arg=7, z_arg=8)
  expect_that(scope(6, 7, 8), equals(cmp))
  expect_that(scope_e(), equals(cmp))
  expect_that(scope_d(), equals(cmp))

  x_testthat <- -2
  cmp[2] <- x_testthat
  expect_that(scope(6, 7, 8), equals(cmp))
  expect_that(scope_e(), equals(cmp))
  expect_that(scope_d(), equals(cmp))
})
