#include "util.hpp"

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

// Given a vector-of-vectors, copy the vector x[i] into the ith
// *column* of an Rcpp matrix.
Rcpp::NumericMatrix 
to_rcpp_matrix_by_col(const std::vector< std::vector<double> >& x) {
  const size_t nc = x.size();
  if (nc == 0) {
    Rcpp::stop("Can't make matrix of empty vector");
  }
  Rcpp::NumericMatrix ret(static_cast<int>(x.begin()->size()),
                          static_cast<int>(nc));
  Rcpp::NumericMatrix::iterator it = ret.begin();
  for (size_t i = 0; i < nc; ++i) {
    it = std::copy(x[i].begin(), x[i].end(), it);
  }
  return ret;
}

Rcpp::NumericMatrix 
to_rcpp_matrix_by_row(const std::vector< std::vector<double> >& x) {
  const size_t nr = x.size();
  if (nr == 0) {
    Rcpp::stop("Can't make matrix of empty vector");
  }
  Rcpp::NumericMatrix ret(static_cast<int>(nr),
                          static_cast<int>(x.begin()->size()));
  for (size_t i = 0; i < nr; ++i) {
    Rcpp::NumericMatrix::Row r = ret(i, Rcpp::_);
    std::copy(x[i].begin(), x[i].end(), r.begin());
  }
  return ret;
}

size_t safe_size_t_from_r(int n) {
  if (n < 0) {
    Rcpp::stop("Required a non-negative value");
  }
  return static_cast<size_t>(n);
}

}

}
