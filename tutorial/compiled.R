## # A very brief introduction to compiled rodeint functions
library(rodeint)
library(lattice)
library(Rcpp)

## The way that `rodeint` deals with compiled system is quite
## different from `deSolve`, and uses the expressiveness of Rcpp to
## easily define a problem.

## It's really not possible to write a differential equation solver
## without using the lorenz system, so here it is, in R code.
lorenz <- function(y, t, pars) {
  sigma <- pars[["sigma"]]
  R <- pars[["R"]]
  b <- pars[["b"]]
  c(sigma * (y[2] - y[1]),
    R * y[1] - y[2] - y[1] * y[3],
    -b * y[3] + y[1] * y[2])
}

## Here are the classical parameters:
pars <- c(sigma = 10.0,
          R     = 28.0,
          b     =  8.0 / 3)

## We can build the system this way:
sys_r <- ode_system(lorenz, pars)

## Initial conditions
y0 <- c(10.0, 1.0, 1.0)

f_r <- make_integrate(sys_r, y=y0, t0=0, dt=0.1)
y <- f_r(25, TRUE)

## Here, after some wrangling (will get better!) is the classic
## attractor:
##+ lorenz_attractor
d <- as.data.frame(attr(y, "y"))
names(d) <- c("x", "y", "z")
cloud(z ~ x + y, d, type="l", screen=list(x=120, y=40, z=10),
      xlab="", ylab="", zlab="",
      scales=list(draw=FALSE),
      par.box=list(col=NA))

## The translation into C++ is fairly straightforward, but in two
## parts.

code <-
'// [[Rcpp::depends(rodeint)]]
#include <rodeint_ode_system.h>
void lorenz_derivs(const rodeint::vector_stl& y,
                   rodeint::vector_stl& dydt,
                   double /* t */,
                   const rodeint::vector_stl& pars) {
  const double sigma = pars[0];
  const double R = pars[1];
  const double b = pars[2];
  dydt[0] = sigma * (y[1] - y[0]);
  dydt[1] = R * y[0] - y[1] - y[0] * y[2];
  dydt[2] = -b * y[2] + y[0] * y[1];
}

// [[Rcpp::export]]
rodeint::ode_system_cpp
lorenz_cpp(rodeint::vector_stl pars) {
  rodeint::util::check_length(pars.size(), 3);
  return rodeint::ode_system_cpp(&lorenz_derivs, pars);
}
'

## There's a bunch of stuff going on here.  First, lines beginning
## with '// [[Rcpp::' are special to Rcpp - they'll be handled by
## sourceCpp.  The first line saying
##
## ```
## // [[Rcpp::depends(rodeint)]]
## ```
##
## will set up the include path for us, but we still need to include
## the header file "rodeint_ode_system.h".
##
## The line
##
## ```
## // [[Rcpp::export]]
## ```
##
## means that sourceRcpp will arrange to *export* that function to R
## for us - we'll be able to call it.

## The first function (`lorenz_derivs`) is the actual derivatives
## function.  The types here are *very* strict (much stricter than
## odeint is).
##
## You must declare 'y' as `const rodeint::vector_stl&`, which is the
## same thing as `const std::vector<double>&`.  You can access
## elements in `y`, but cannot change them without making a copy.
## Because it's passed by reference this should be very fast.
##
## `dydt` is where the derivatives will be stored, and must be passed
## as `std::vector<double>&` - we're adding derivatives by reference
## again.
##
## time is simply a double -- because we're not using it here I've
## commented the name out to avoid a compiler warning.
##
## The parameters are also passed as a const reference.  If you want
## to change them you'll need to explicitly copy them.
##
## The function returns void - all the output is done by writing to
## the `dydt` parameter.
##
## The rest of `lorenz_derivs` is straightforward - unpack the
## parameters and compute each of the derivatives.
##
## The second function does the magic.  The return type
## `rodeint::ode_system_cpp` corresponds to a rodeint object for
## making ode systems from C++ target functions (other types are
## `ode_system_r` which is what R functions become and
## `ode_systems_class` which will be explained below).
##
## The function name `lorenz_cpp` will be exported to R.  This is
## going to take an argument `pars`, which must be
## `rodeint::vector_stl` -- this will be the initial parameter value.
## The next line checks that the parameters are the expected length
## (3), and is optional.
##
## The final line puts it together - it takes the address of the
## derivatives function, the provided parameters and constructs an
## `ode_system_cpp` object.

## Rcpp organises compiling the code and loading it into R:
sourceCpp(code=code)

## And the `lorenz_cpp` function now exists.
exists("lorenz_cpp")

## Running the returned function gives quite peculiar output:
lorenz_cpp(pars)

## The way to actually use this function is to pass it to `ode_system`
## *in place* of the `derivs` function:
sys_cpp <- ode_system(lorenz_cpp, pars)
print(sys_cpp)

## This can then be used just like `sys` above:
sys_r$derivs(y0, 0)
sys_cpp$derivs(y0, 0)

## And can be passed around exactly the same way as the R-based
## target.  Once the system is built, it will not be obvious if this
## is a compiled or R-based system.
f_cpp <- make_integrate(sys_cpp, y=y0, t0=0, dt=0.1)
y_cpp <- f_cpp(25, TRUE)

## Gives the same answer as the R version
all.equal(y, y_cpp)

## However it is quite a bit quicker.
system.time(f_r(25, TRUE))
system.time(f_cpp(25, TRUE))

## The only way of telling what sort of target you have is to use the
## `show` function with the optional argument `details=TRUE`:
sys_r$show(details=TRUE)
sys_cpp$show(details=TRUE)

## ...or by accessing the "type" element:
sys_r$type
sys_cpp$type

## The same system can be defined in a class-based way.

code <-
'// [[Rcpp::depends(rodeint)]]
#include <rodeint_ode_system.h>
class lorenz {
public:
  typedef SEXP pars_type;
  double sigma, R, b;
  lorenz(SEXP pars) {
    set_pars(pars);
  }
  void set_pars(SEXP pars) {
    const std::vector<double> p = Rcpp::as<std::vector<double> >(pars);
    rodeint::util::check_length(p.size(), 3);
    sigma = p[0];
    R     = p[1];
    b     = p[2];
  }
  void derivs(const rodeint::vector_stl& y, rodeint::vector_stl& dydt,
              const double /* t */) {
    dydt[0] = sigma * (y[1] - y[0]);
    dydt[1] = R * y[0] - y[1] - y[0] * y[2];
    dydt[2] = -b * y[2] + y[0] * y[1];
  }
};

// [[Rcpp::export]]
rodeint::ode_system_class
lorenz_class(SEXP pars) {
  return rodeint::make_ode_system_class<lorenz>(pars);
}
'

## The requirements here are:
## 1. A function `set_pars` with the same signature as the above (void
## set_pars(SEXP)).  You can use any Rcpp-based wrangling you want to
## convert the argument from a SEXP to something useful.  Here, I'm
## using `Rcpp::as` to convert it into a stl vector, checking that
## there are three elements and then assigning parameters to the three
## named fields.  If the parameter setting involves potentially costly
## pre-calculation, you can do that here and avoid recomputing things
## in the derivative function over and over.
##
## 2. A function `derivs` with the same signature as the above.  This
## is essentially the same signature described by the `odeint`
## documentation, because we no longer need to pass parameters in.
## Note that even though this method could perhaps be declared `const`
## (as written it does not change anything in the class) it may not be
## declared const to work with the current rodeint implementation.
## This may relax in the future.
##
## 3. A constructor that takes a single object of class `SEXP` as
## parameters.  It's probably easiest to just forward that to
## `set_pars`, as is done here.
##
## 4. The public type declaration `typedef SEXP pars_type;` This is to
## allow more flexible automatic parameter wrangling in an upcoming
## version.

## As with the free function version, the actual interface code is
## small and fairly cryptic.  The Rcpp::export flag lets sourceCpp
## know that this is a function that we want exported.  It again must
## take a single argument 'pars' with type SEXP and returns an object
## of class `rodeint::ode_system_class`.  All that is needed is to
## call the rodeint::make_ode_system_class with the name of your class
## as a template parameter and the parameters object as the only
## argument.

## With a bit of faffing about, the requirement of names for the two
## functions above is dropped, and the constructor may have a
## different signature.  The initialisation allowing for arbitrary
## constructors:
##
## ```
## // [[Rcpp::export]]
## rodeint::ode_system_class
## lorenz_class(SEXP pars) {
##   lorenz obj(pars);
##   return rodeint::make_ode_system_class<lorenz,
##                                         &lorenz::derivs,
##                                         &lorenz::set_pars>(obj, pars);
## }
## ```
##
## The difference here is that:
## 1. The object is manually initialised.
## 2. The pointers to methods are explicitly given for *both* `derivs`
## and `set_pars`.  Once this is done the name is irrelevant.
## 3. The manually initialised object `obj` is passed in to
## `make_ode_system_class` as the first argument `pars` is still
## needed, even though the object is initialised already.

sourceCpp(code=code)

## The `lorenz_class` function does exist:
exists("lorenz_class")

## As with the free function version, calling it gives very cryptic
## output:
lorenz_class(pars)

## That is because this is also a generator function to pass into
## `ode_system`.
sys_class <- ode_system(lorenz_class, pars)

## We now have three version of the Lorenz system that are built from
## different components but will act the same way:
sys_r$derivs(y0, 0)
sys_cpp$derivs(y0, 0)
sys_class$derivs(y0, 0)

f_class <- make_integrate(sys_class, y=y0, t0=0, dt=0.1)
y_class <- f_class(25, TRUE)

## Gives the same answer as the R version
all.equal(y, y_class)

## The cpp version appears the fastest of the three on this probem.
## There is some fairly heavy redirection going on with the class
## version, and I suspect that the difference there accounts for most
## of the difference (and that it's probably possible to improve the
## class version).  However, for problems with a lot of potential for
## caching calculations the class-based version may end up being
## faster.
system.time(f_r(25, TRUE))
system.time(f_cpp(25, TRUE))
system.time(f_class(25, TRUE))
