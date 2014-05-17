##' These functions, and the class \code{stepper} create a "stepper"
##' object for solving a system of ordinary differential equations
##' created with \code{\link{ode_system}}.  They cannot yet be used
##' for directly manually stepping a system of ODEs, but essentially
##' act as placeholders collecting information about algorithms.
##'
##' There are three "categories" of stepper.  These are the broadest
##' class of distinctions between approaches
##'
##' \itemize{
##' \item \code{basic}: step with a fixed step size
##' \item \code{controlled}: step with a step size that is tuned based
##' on errors reported by the stepper
##' \item \code{dense}: as for \code{controlled}, but it can make use
##' of "dense output" to interpolate points \emph{between} steps where
##' needed.
##' }
##'
##' These correspond to the concepts "Stepper", "Controlled Stepper",
##' and "Dense Output Stepper" in the
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/odeint_in_detail/steppers.html}{odeint
##' documentation}.
##'
##' Within steppers there are "algorithms" - these are the
##' mathematical rules used to advance the system.  Almost all the
##' algorithms in odeint are supported by rodeint.  There are many
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/odeint_in_detail/steppers.html#boost_numeric_odeint.odeint_in_detail.steppers.stepper_overview}{possible
##' algorithms}!
##'
##' The functions \code{stepper_basic_algorithms},
##' \code{stepper_controlled_algorithms} and
##' \code{stepper_dense_algorithms} return vectors of valid
##' algorithms.
##'
##' The \code{runge_kutta_dopri5} stepper is described by \code{odint}
##' as possibly "the best default stepper", so probably start with
##' that and see the \code{odeint} documentation for when the other
##' types might be more appropriate.
##'
##' If your system has a Jacobian associated with it, you can also use
##' the \code{rosenbrock4} algorithm (for any of the three
##' categories).
##'
##' Steppers in the "controlled" and "dense" categories adjust their
##' step size according the detected error, in an effort to take as
##' big a step as possible while keeping the error to within some
##' specified bounds.  The precise that this affects the stepper
##' depends on the algorithm, and is described in the
##' \href{http://headmyshoulder.github.io/odeint-v2/doc/boost_numeric_odeint/odeint_in_detail/steppers.html#boost_numeric_odeint.odeint_in_detail.steppers.controlled_steppers}{odeint
##' documentation}.  The parameter \code{abs_tol} changes the
##' tolerance for \emph{absolute} error while \code{abs_tol} changes
##' the tolerance for \emph{relative} error.  For the Runge Kutta
##' steppers the interpretation is similar, though I believe not
##' identical, to the steppers in \code{deSolve}.
##'
##' Some algorithms in odeint are not supported yet:
##' \itemize{
##' \item Any of the multistep steppers (\code{adams_bashforth},
##' \code{adams_moulton}, \code{adams_bashforth_moulton}) because
##' these require explicit initialisation from another stepper and so
##' represent an interface challenge.  However, they are apparently
##' good for expensive functions, so might end up in the package soon.
##' \item Any of the "symplectic" steppers (\code{symplectic_euler},
##' \code{symplectic_rkn_sb3a_mclachlan},
##' \code{symplectic_rkn_sb3a_m4_mclachlan}) because I've never done
##' any work with Hamiltonian systems.  If these are implemented they
##' will get a new category.
##' \item The \code{velocity_verlet} stepper for second order systems.
##' \item The \code{implicit_euler} stepper for stiff systems.
##' }
##'
##' @examples
##'
##' ## The three stepper categories are:
##' stepper_categories()
##'
##' ## There are two ways of listing valid algorithms for a particular
##' ## category: using stepper_algorithms(category):
##' stepper_algorithms("controlled")
##'
##' ## ...or using stepper_<category>_algorithms()
##' stepper_controlled_algorithms()
##'
##' ## If you add have_jacobian=TRUE to these calls you'll get the
##' ## "rosenbrock4" stepper added to the list
##' stepper_algorithms("controlled", have_jacobian=TRUE)
##'
##' ## To build a stepper use the "make_stepper" function:
##' s <- make_stepper("controlled", "runge_kutta_dopri5")
##' print(s)
##'
##' ## The tolerances can be adjusted by passing in optional arguments
##' ## "abs_tol" and "rel_tol"
##' s <- make_stepper("controlled", "runge_kutta_dopri5",
##'                   abs_tol=1e-4, rel_tol=1e-8)
##' print(s)
##'
##' @title Create ODE Stepper
##' @aliases stepper
##' @rdname stepper
##' @export
stepper <- setRefClass("stepper",
                       fields=list(
                         category="character",
                         algorithm="character",
                         ublas_state="logical",
                         abs_tol="numeric",
                         rel_tol="numeric",
                         ptr="externalptr"))
stepper$lock(setdiff(names(stepper$fields()), "ptr"))

stepper$methods(show = function(details=FALSE) {
  cat("A stepper for solving ordinary differential equations\n\n")
  cat("This object has no useful methods\n")
  cat("Pass this stepper to functions in ?rodeint_integrate\n")
  cat("\nDetails:\n")
  cat(sprintf("\tcategory: %s\n", category))
  cat(sprintf("\talgorithm: %s\n", algorithm))
  if (category != "basic") {
    cat(sprintf("\tabs_tol: %s\n", abs_tol))
    cat(sprintf("\trel_tol: %s\n", rel_tol))
  }
  if (details) {
    cat("----------------------------\n")
    cat(sprintf("addr:  %s\n", ptr_address(ptr)))
    cat(sprintf("ublas_state: %s\n",
                if (ublas_state) "yes" else "no"))
    cat("----------------------------\n")
  }
})

stepper$methods(initialize=function(category, algorithm, has_jacobian,
                  abs_tol, rel_tol) {
  category <<- category
  algorithm <<- algorithm
  ublas_state <<- has_jacobian
  abs_tol <<- abs_tol
  rel_tol <<- rel_tol
  ptr <<- stepper__ctor(category, algorithm, ublas_state,
                        abs_tol, rel_tol)
})

stepper$methods(rebuild = function() {
  ptr <<- stepper__ctor(category, algorithm, ublas_state,
                        abs_tol, rel_tol)
})

stepper$methods(details = function() {
  stepper__details(ptr)
})

## This is going to change at some point, but this is a list of the
## possible controlled stepper algorithms.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.

##' Valid values for making a \code{\link{stepper}}.
##' @title Lists of Stepper Algorithms
##' @author Rich FitzJohn
##' @export
##' @rdname stepper
##' @param category Either "basic" or "controlled"
##' @param have_jacobian Logical indicating if the stepper will work
##' on problems with a Jacobian (this is a superset: every problem
##' that provides a Jacobian will work fine with steppers that don't
##' require a Jacobian).
stepper_algorithms <- function(category, have_jacobian=FALSE) {
  switch(category,
         basic=stepper_basic_algorithms(have_jacobian),
         controlled=stepper_controlled_algorithms(have_jacobian),
         dense=stepper_dense_algorithms(have_jacobian),
         stop("Invalid stepper category"))
}

##' @export
##' @rdname stepper
stepper_basic_algorithms <- function(have_jacobian=FALSE) {
  c("euler",
    "modified_midpoint",
    "runge_kutta4",
    "runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5",
    if (have_jacobian) "rosenbrock4")
}

##' @export
##' @rdname stepper
stepper_controlled_algorithms <- function(have_jacobian=FALSE) {
  c("runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5",
    "bulirsch_stoer",
    if(have_jacobian) "rosenbrock4")
}

##' @export
##' @rdname stepper
stepper_dense_algorithms <- function(have_jacobian=FALSE) {
  c("euler",
    "runge_kutta_dopri5",
    "bulirsch_stoer",
    if(have_jacobian) "rosenbrock4")
}

##' @export
##' @rdname stepper
stepper_categories <- function() {
  c("basic", "controlled", "dense")
}

##' @rdname stepper
##' @export
make_stepper_basic <- function(algorithm, abs_tol=NA_real_, rel_tol=NA_real_,
                               has_jacobian=NA) {
  if (!is.na(abs_tol) || !is.na(rel_tol)) {
    warning("Ignoring provided tolerance arguments")
  }
  if (is.na(has_jacobian)) {
    has_jacobian <- algorithm == "rosenbrock4"
  }
  stepper("basic", algorithm, has_jacobian, NA_real_, NA_real_)
}

## TODO: See the comment about has_jacobian below.
##' @rdname stepper
##' @export
##' @param abs_tol Absolute tolerance (see odeint docs for now)
##' @param rel_tol Relative tolerance (see odeint docs for now)
##' @param has_jacobian Logical, indicating on whether the stepper
##' should use the internal data structures required by the stiff
##' systems.  This is likely to disappear soon, as it depends entirely
##' on the system itself.  The default, NA, will switch based on the
##' \code{algorithm} argument -- \code{rosenbrock4} is the stiff system
##' stepper.  However, to use other steppers with the stiff system, we
##' need to set up normal steppers similarly.  So probably at runtime
##' this will just rebuild the stepper for us.
make_stepper_controlled <- function(algorithm, abs_tol=1e-6, rel_tol=1e-6,
                                    has_jacobian=NA) {
  if (is.na(has_jacobian)) {
    has_jacobian <- algorithm == "rosenbrock4"
  }
  stepper("controlled", algorithm, has_jacobian, abs_tol, rel_tol)
}

##' @rdname stepper
##' @export
make_stepper_dense <- function(algorithm, abs_tol=1e-6, rel_tol=1e-6,
                               has_jacobian=NA) {
  if (is.na(has_jacobian)) {
    has_jacobian <- algorithm == "rosenbrock4"
  }
  stepper("dense", algorithm, has_jacobian, abs_tol, rel_tol)
}

##' @rdname stepper
##' @export
##' @param category Either "basic" or "controlled"
##' @param algorithm The stepper algorithm (e.g. "runge_kutta4") - see
##' lists in \code{\link{stepper_algorithms}}
##' @param ... Additional parameters passed from \code{make_stepper}
##' to either \code{make_stepper_basic} (none allowed) or
##' \code{make_stepper_controlled}.
make_stepper <- function(category, algorithm, ...) {
  make <- switch(category,
                 basic=make_stepper_basic,
                 controlled=make_stepper_controlled,
                 dense=make_stepper_dense,
                 stop("Invalid stepper category"))
  make(algorithm, ...)
}
