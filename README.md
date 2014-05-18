# rodeint

[![Build Status](https://travis-ci.org/richfitz/rodeint.png?branch=master)](https://travis-ci.org/richfitz/rodeint)

![rodeint logo](https://github.com/richfitz/rodeint/raw/master/extra/rodeint.png)

An R interface to [odeint](http://headmyshoulder.github.io/odeint-v2/downloads.html), using [Rcpp](http://www.rcpp.org/) for glue.

## Documentation
Reference documentation is available here [here](http://richfitz.github.io/rodeint/) (generated with [staticdocs](https://github.com/hadley/staticdocs)).  A tutorial introduction to making compiled systems is available [on the wiki](https://github.com/richfitz/rodeint/wiki/compiled).  General tutorial documentation is not available yet.

As an example of what I'm aiming for, [here](https://github.com/richfitz/rodeint/blob/master/inst/examples/harmonic_oscillator_cpp.cpp) is a simple harmonic oscillator, implemented as a function.  The embedded R code at the bottom will run when passed through `Rcpp::sourceCpp`.

Similarly, [here](https://github.com/richfitz/rodeint/blob/master/inst/examples/harmonic_oscillator_class.cpp) is the same system implemented as a small class.  The book-keeping here is slightly greater, but allows for arbitrary calculations after parameters are set (e.g., precomputing values that would be used at all times).

## Status

The package is not yet stable, but most features are now in place aside from better deSolve support and low level stepper features.
