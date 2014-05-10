## Ignoring type checking for now.

##' Partially apply arguments to a function, reducing the number of
##' arguments it takes, or setting them as defaults.
##'
##' There are two strategies.  In the default, with
##' \code{set_as_defaults=FALSE}, we assign all supplied values into a
##' new environment that is placed ahead of the function's native
##' environment: the supplied values will be found first.  The
##' returned function will have an argument list that omits the given
##' arguments.
##'
##' In the second, with \code{set_as_defaults=TRUE}, we set the
##' supplied values as defaults in the function's argument list.  The
##' returned function's argument list still includes the given
##' arguments, but they are all moved \emph{after} the unset values,
##' and kept in the order provided.  If the function contains
##' \code{...} as an argument, then these values will be put
##' \emph{after} the ellipsis, which means partial name matching won't
##' work (don't use it anyway!).
##'
##' Both strategies seem to offer similar performance.  I suspect that
##' the \code{set_as_defaults=TRUE} strategy is a little more fragile.
##' They differ mainly in how the function is intended to be used - if
##' you want to forever specify particular values for a value, use
##' \code{set_as_defaults=FALSE}.  If you want to generate new
##' function that you might sometimes want to use different values in,
##' use \code{set_as_defaults=TRUE}.
##'
##' @title Partially Apply Arguments to a Function
##' @param f A function
##' @param ... Named arguments to set
##' @param set_as_defaults Should values in \code{...} be set as
##' defaults?
##' @author Rich FitzJohn
##' @export
partially_apply <- function(f, ..., set_as_defaults=FALSE) {
  if (is.primitive(f)) {
    stop("Cannot partially apply primitive functions")
  }
  dots <- list(...)

  ## Early exit -- this seems safest.
  if (length(dots) == 0) {
    return(f)
  }

  assert_named(dots)
  nf <- names(formals(f))
  nd <- names(dots)

  if (!all(nd %in% nf)) {
    stop("Unknown arguments: ", collapse(setdiff(nd, nf)))
  }
  if (any(duplicated(nd))) {
    stop("Duplicated formal arguments ",
         collapse(unique(nd[duplicated(nd)])))
  }

  ## Both of the strategies clears attributes, so store them here.
  old_attributes <- attributes(f)

  if (set_as_defaults) {
    f <- partially_apply_defaults(f, dots)
  } else {
    f <- partially_apply_environment(f, dots)
  }

  ## Reset any attributes that might have been cleared, except for
  ## srcref, which is now invalid (and will confusingly be printed
  ## with the wrong structure).
  attributes(f) <- old_attributes[names(old_attributes) != "srcref"]

  f
}

## The default-setting strategy, based on diversitree::set.defaults().
##
## This assumes that
partially_apply_defaults <- function(f, defaults) {
  to_set <- names(defaults)
  ff <- formals(f)
  ## Reorder the formals:
  ff <- ff[c(setdiff(names(ff), to_set), to_set)]
  ## Set our new defaults:
  ff[to_set] <- defaults
  ## And push back onto functions
  formals(f) <- ff
  f
}

## An environment based strategy
partially_apply_environment <- function(f, defaults) {
  ff <- formals(f)
  formals(f) <- ff[c(setdiff(names(ff), names(defaults)))]

  e <- as.environment(defaults)
  parent.env(e) <- environment(f)
  environment(f) <- e

  f
}
