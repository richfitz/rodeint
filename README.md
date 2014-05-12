# rodeint

[![Build Status](https://travis-ci.org/richfitz/rodeint.png?branch=master)](https://travis-ci.org/richfitz/rodeint)

![rodeint logo](https://github.com/richfitz/rodeint/raw/master/extra/rodeint.png)

An R interface to [odeint](http://headmyshoulder.github.io/odeint-v2/downloads.html), using Rcpp for glue.

This is under development and nothing is guaranteed to stay the same for any length of time.  Use at your own risk!

As an example of what I'm aiming for, [here](https://github.com/richfitz/rodeint/blob/master/inst/examples/harmonic_oscillator_cpp.cpp) is a simple harmonic oscillator, implemented as a function.  The embedded R code at the bottom will run when passed through `Rcpp::sourceCpp`.

Similarly, [here](https://github.com/richfitz/rodeint/blob/master/inst/examples/harmonic_oscillator_class.cpp) is the same system implemented as a small class.  The book-keeping here is slightly greater, but allows for arbitrary calculations after parameters are set (e.g., precomputing values that would be used at all times).

The `make_integrate` function binds together target functions, integration backends and steppers.  It creates functions with relatively few arguments that are tailored to the way the function will be used.

Documentation as the package evolves available [here](http://richfitz.github.io/rodeint/) (generated with [staticdocs](https://github.com/hadley/staticdocs)).
