% Generated by roxygen2 (4.0.0): do not edit by hand
\docType{class}
\name{ode_system-class}
\alias{ode_system}
\alias{ode_system-class}
\title{Systems of ODEs}
\description{
See the documenation for \code{initialize} for creating, and
generally methods below.  Examples and proper documentation
coming.
}
\section{Methods}{

\describe{
\item{\code{derivs(y, t)}}{Compute the derivatives.  The first argument (\code{y}) is the
system state and the second (\code{t}) is the time.  No validation is
done on either variable.  This might change in the future.}

\item{\code{get_pars()}}{Extract parameters from the object.  If you are using a class-based
ode_system, this is the parameters passed in to the object and is not
necessarily the same as the way you are choosing to store processed
parameters.}

\item{\code{initialize(derivs, pars, jacobian = NULL, validate = NULL,
  deSolve_style = FALSE)}}{\itemize{
\item \code{derivs} Either
\itemize{
\item an R function of three parameters \code{(y, t, pars)} that
returns a vector of derivatives (dy/dt), the same length as \code{y}.
\item as R function that, when run with the single argument
\code{pars} returns a special object that will point at compiled
functions (\code{ode_system_cpp}) or a compiled class
\code{ode_system_class}. There will be more extensive documentation
coming on this soon.
}
\item \code{pars} the initial parameters, as accepted by your
function.  Generally this will be a numeric vector, but see
\code{set_pars}.  This exists so that we can establish the correct
parameter vector length, and to ensure that all systems have
\emph{something} initialised to prevent possible crashes or
unexpected behaviour.
\item \code{jacobian} If you are specifiying an R system and have
the jacobian, then this allows you to use stiff solvers
(e.g. \code{rosenbrock4}).  It needs to be an R function of three
parameters \code{(y, t, pars)} and return the Jacobian matrix.
\item \code{validate} optional function to run on parameters each
time they are set (see \code{set_pars}).  Useful for checking length,
positivity, etc.  The function should throw an error (stop) if the
parameters are not valid.  The return value of this function is
ignored.
\item \code{deSolve_style} Set this to \code{TRUE} if your R function
is in deSolve style rather than rodeint style (i.e., parameters
\code{(t, y, pars)} and returning a \emph{list} with the first
element being the derivatives).
}}

\item{\code{jacobian(y, t)}}{Compute the Jacobian.  Not all systems can do this}

\item{\code{set_pars(pars)}}{Set parameters within the object.

In addition to the validation carried out by \code{validate},
different targets check in different ways (you only need to know about
this if you are writing targets):
\itemize{
\item \code{ode_system_t}: No checking is done.  You can of course
check within the derivatives function.

\item \code{ode_system_cpp}: The parameters must be a numeric vector the
same length as the currently set parameters (so by extension the set
that it was initialised).  If you need variable length parameters you
will need to write a class. This might change once we have validation
functions.

\item \code{ode_system_class}: Parameters will be changed using whatever
you implemented in the \code{set_pars} function, including what is
impled by the conversion using \code{Rcpp::as<your_type>(SEXP)}.
}}
}}
\section{Warning}{


For performance reasons, we do not check that the function
returned the correct length of derivatives.  If your function
returns fewer than the correct number you'll get uninitialised
values (which will cause the integration to fail) and if you
return too many then you can crash R.  I might add better
checking in a future version.
}
\examples{
## Define a harmonic oscillator.  If you've used deSolve, beware that
## the function here differs in two respects: the order of the
## arguments 'y' and 't' are different (we take 'y' first) and the
## function does not return a list, but just the vector of
## derivatives.  These changes are to match the design of the odeint
## package.
derivs <- function(y, t, pars) {
  c(y[2],
    -y[1] - pars * y[2])
}

## You also must provide a starting parameter vector.
pars <- 0.5

## Here is the system of differential equations:
sys <- ode_system(derivs, pars)

## You can call the derivative function (not that you'll need to).  It
## no longer takes a parameter argument, as that is kept within the
## object.
y <- c(0, 1)
t <- 0
sys$derivs(y, t)
derivs(y, t, pars)

## You can ask what parameters are stored in the object using the
## "get_pars()" method
sys$get_pars()

## And set new parameters with "set_pars()"
sys$set_pars(pi)
sys$derivs(y, t)

## Running sys$show() or print(sys) (or just writing sys) will tell
## you a little about the object
print(sys)

## It's important to know that these are "reference class" objects.
## They don't obey R's usual copy semantics.  So if you do
sys2 <- sys

## sys2 and sys are the *same* object.  If you set the parameers in
## one it will set them in the other:
sys2$set_pars(1)
sys2$get_pars() # these have the same
sys$get_pars()  # parameters now

## To make an independent copy, use the "copy()" method
sys2 <- sys$copy() # sys2 is now independent from sys

sys2$set_pars(2)
sys2$get_pars() # these have *different*
sys$get_pars()  # parameters now

## If you know the Jacobian you can use that information with some
## solvers.  The entry [i,j] in the Jacobian must be the derivative of
## the ith equation with respect to the jth variable.
jacobian <- function(y, t, pars) {
  rbind(c(0,  1),
        c(-1, pars))
}

## Build the system like this:
sys <- ode_system(derivs, pars, jacobian)

## Now print(sys) will tell you that there is a jacobian available.
## You can call it the same way as derivs
sys$jacobian(y, t)
jacobian(y, t, pars)

## You can also specify a "validator" for parameters.  This will check
## that the parameters are reasonable before setting them.  If it
## fails then parameters will not be set and will remain unmodified.
## The function must take a single (the new parameters) and must throw
## an error if it fails.  It need not return anything.
##
## For example, this function will check that all parameters are
## non-negative.
assert_nonnegative <- function(pars) {
  if (all(pars < 0)) {
    stop("Parameters must be non-negative")
  }
}

## Then build the system as before.
sys <- ode_system(derivs, pars, validate=assert_nonnegative)

\dontrun{
## Then if negative parameters are set we get an error
try(sys$set_pars(-1))
## ...and the original parameters are unchanged:
sys$get_pars()
}

## It is possible to directly use functions that were developed for
## use with deSolve.  The deSolve-style equivalent of the above
## derivatives function looks like this:
derivs_deSolve <- function(t, y, pars) {
  dydt <- c(y[2],
            -y[1] - pars * y[2])
  list(dydt)
}

## Using the argument "deSolve_style" we can tell ode_system that this
## is a deSolve-style function:
sys_deSolve <- ode_system(derivs_deSolve, pars, deSolve_style=TRUE)

## This will now behave in the way that rodeint expects systems to
## behave and can be used as if it was not a deSolve style function.
sys_deSolve$derivs(y, t)
sys$derivs(y, t)

## Notice that the y and t arguments are now in odeint order.

## Using compiled systems is beyond the scope of the examples.  A
## tutorial is coming soon.
}
\seealso{
\code{\link{integrate_adaptive}}, which documents
different ways of integrating objects generated by this function.
}

