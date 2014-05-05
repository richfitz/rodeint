# rodeint

[![Build Status](https://travis-ci.org/richfitz/rodeint.png?branch=master)](https://travis-ci.org/richfitz/rodeint)

![rodeint logo](https://github.com/richfitz/rodeint/raw/master/extra/rodeint.png)


R interface to [odeint](http://headmyshoulder.github.io/odeint-v2/downloads.html), using Rcpp for glue.

Pre-issues:

- [x] Decide on a better name? (*actually keep this name and then we can have a logo which is a rat with an integral sign on its body*)
- [x] Decide on the version of odeint to target, and a mechanism for tracking upstream (*Looks like BH has a decent version of odeint (2.2) as of 1.54.0-1, so I'm going to use that until I know I need something else*)
- [x] Implement all examples pointing at [BH](http://cran.r-project.org/web/packages/BH/index.html) to confirm that we have all the required headers, gather any others needed (*No longer the idea, I think*)
- [x] Write simple handler for integrating derivatives written in R
- [ ] Extend to a C++ API that is accessible via include files.  This should be fairly straightforward given that odeint is header only.
- [ ] Benchmarks against gsl and lsoda - can use the gsl code in diversitree, and lsoda code in deSolve.

# Boost and odeint version

Using odeint-v2 version v2.2 (based on the repo tag) and the current BH (version 1.54.0).

Using v2.3 doesn't work because of boost/multi_array.hpp being needed, and version v2.4 appears to be a more major restructing.  I've not extensively tested the examples though, and only lorenz.cpp appears to work.

However, some of odeint appears included in BH, so it's possible that we can just use that and save a bunch of hassle.

# Approach

No modules this time - try getting Rcpp attributes working perhaps?

Things I need:

* Ability to integrate an R function with a constrained interface.  This might be the place to start, actually.

* Ability to switch between a few different schemes.

* The functor version is easy enough to just directly use the odeint code as-is.  Would be nice to have a version that eases pointing at a 'derivs' member function though.  See
http://www.boost.org/doc/libs/1_53_0/libs/numeric/odeint/examples/bind_member_functions.cpp
http://www.boost.org/doc/libs/1_53_0/libs/numeric/odeint/examples/bind_member_functions_cpp11.cpp

* Differentiate between cases where we want to integrate the system (`integrate_adaptive`) and where we want to collect information along the way (`integrate_times`)

The plan:

1. Get a very basic system going in a class based approach, using `integrate()`, `integrate_times()` and `integrate_adaptive()`.
2. Set up parallel system using deSolve (easy peasy)
3. Modify the C++ version to use Rcpp to accept an R function and some parameters
4. Start getting that working with a few different integration backends.

The desirable outcome for forest/diversitree is a fully self contained "integrator" object that will take initial conditions and times and spit back end points (running in single or multi mode).  I'm happy to have a few fairly non-generic types (e.g. one for rk) though that'd be better to be wrapped up.

At the underlying level, I guess we need at least one for an R function and one for a C function that takes a vector<double> for parameters and state.  It's probably going to be most natural to write that with templates though.
