#ifndef _RODEINT_UTIL_HPP_
#define _RODEINT_UTIL_HPP_

#include <Rcpp.h>
#include <vector>
#include <sstream> // std::stringstream

namespace rodeint {

void check_dt(double t0, double t1, double dt);

namespace util {

Rcpp::NumericMatrix 
to_rcpp_matrix_by_col(const std::vector< std::vector<double> >& x);
Rcpp::NumericMatrix
to_rcpp_matrix_by_row(const std::vector< std::vector<double> >& x);
void check_length(size_t received, size_t expected);

template<typename T>
std::string to_string(T x) {
  std::ostringstream o;
  if (!(o << x))
    Rcpp::stop("String conversion failure");
  return o.str();
}

}
}

#endif
