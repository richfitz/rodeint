#include <rodeint/util.hpp>

namespace rodeint {

// Cases:
//   * t1 > t0, dt > 0 -- OK: classical forward time
//   * t1 < t0, dt < 0 -- OK: backward time
//   * t1 > t0, dt < 0 -- ERR: dt is going the wrong way
//   * t1 < t0, dt < 0 -- ERR: dt is going the wrong way
// If time has no difference we don't care about the sign of dt,
// especially because R does not have a negative zero.
//   * t1 = t0, dt > 0 -- OK: nothing will happen
//   * t1 = t0, dt < 0 -- OK: nothing will happen
// The first of these is technically possible but odeint fails and I
// don't think it really makes a lot of sense.
//   * t1 = t0, dt = 0 -- ERR: integration not possible
//   * t1 > t0, dt = 0 -- ERR: integration not possible
//   * t1 < t0, dt = 0 -- ERR: integration not possible
void check_dt(double t0, double t1, double dt) {
  if (dt == 0) {
    Rcpp::stop("dt cannot be zero");
  } else if (t0 != t1) {
    const bool t_forwards = t1 > t0, dt_forwards = dt > 0;
    if (t_forwards != dt_forwards) {
      Rcpp::stop("dt has the wrong sign for t0, t1 pair");
    }
  }
}

namespace util {

size_t safe_size_t_from_r(int n) {
  if (n < 0) {
    Rcpp::stop("Required a non-negative value");
  }
  return static_cast<size_t>(n);
}

}

}

// [[Rcpp::export]]
std::string ptr_address(SEXP ptr) {
  void* p = R_ExternalPtrAddr(ptr);
  return rodeint::util::to_string(p);
}

// [[Rcpp::export]]
bool ptr_valid(SEXP ptr) {
  void* p = R_ExternalPtrAddr(ptr);
  return p != NULL;
}
