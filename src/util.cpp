#include "util.hpp"

namespace rodeint {
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

void check_length(size_t received, size_t expected) {
  if (expected != received)
    Rcpp::stop("Incorrect length input; expected " +
               to_string(expected) + ", received " +
               to_string(received));
}

}

}
