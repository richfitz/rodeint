#ifndef _RODEINT_UTIL_UBLAS_HPP_
#define _RODEINT_UTIL_UBLAS_HPP_

namespace rodeint {
namespace util {

template <typename T>
Rcpp::NumericVector
ublas_vector_to_r(const boost::numeric::ublas::vector<T>& v) {
  return Rcpp::NumericVector(v.begin(), v.end());
}

template <typename T>
boost::numeric::ublas::vector<T>
r_vector_to_ublas(const Rcpp::NumericVector& v) {
  boost::numeric::ublas::vector<T> ret(static_cast<size_t>(v.size()));
  std::copy(v.begin(), v.end(), ret.begin());
  return ret;
}

template <typename T>
Rcpp::NumericMatrix
ublas_matrix_to_r(const boost::numeric::ublas::matrix<T>& m) {
  const size_t nr = m.size1(), nc = m.size2();
  Rcpp::NumericMatrix ret(static_cast<int>(nr), static_cast<int>(nc));
  for (size_t i = 0; i < nr; ++i) {
    for (size_t j = 0; j < nc; ++j) {
      ret(i, j) = m(i, j);
    }
  }
  return ret;
}

template <typename T>
boost::numeric::ublas::matrix<T>
r_matrix_to_ublas(const Rcpp::NumericMatrix& m) {
  const size_t
    nr = static_cast<size_t>(m.rows()),
    nc = static_cast<size_t>(m.cols());
  boost::numeric::ublas::matrix<T> ret(nr, nc);
  for (size_t i = 0; i < nr; ++i) {
    for (size_t j = 0; j < nc; ++j) {
      ret(i, j) = m(i, j);
    }
  }
  return ret;
}

}
}

#endif
