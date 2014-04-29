R interface to [odeint](http://headmyshoulder.github.io/odeint-v2/downloads.html), using Rcpp for glue.

Pre-issues:

- [ ] Decide on a better name?
- [ ] Decide on the version of odeint to target, and a mechanism for tracking upstream
- [ ] Implement all examples pointing at [BH](http://cran.r-project.org/web/packages/BH/index.html) to confirm that we have all the required headers, gather any others needed.
- [ ] Write simple handler for integrating derivatives written in R
- [ ] Extend to a C++ API that is accessible via include files.  This should be fairly straightforward given that odeint is header only.
