#ifndef _RODEINT_COMMON_HPP_
#define _RODEINT_COMMON_HPP_

#include <RcppCommon.h>
#include <vector>
#include <boost/numeric/ublas/vector.hpp>

namespace rodeint {

typedef std::vector<double> vector_stl;
typedef boost::numeric::ublas::vector<double> vector_ublas;

namespace util {
inline void stop(const std::string& message) {
  Rcpp::stop(message);
}
}

// inline void warning(const std::string& message) {
//   Rcpp::warning(message);
// }

}

#endif
