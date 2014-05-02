#ifndef _RODEINT_UTIL_HPP_
#define _RODEINT_UTIL_HPP_

#include <Rcpp.h>
#include <vector>

Rcpp::NumericMatrix 
to_rcpp_matrix_by_col(const std::vector< std::vector<double> >& x);
Rcpp::NumericMatrix
to_rcpp_matrix_by_row(const std::vector< std::vector<double> >& x);

#endif
