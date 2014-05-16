#ifndef _RODEINT_STOP_HPP_
#define _RODEINT_STOP_HPP_

#include <RcppCommon.h>

namespace rodeint {
inline void stop(const std::string& message) {
  Rcpp::stop(message);
}

// inline void warning(const std::string& message) {
//   Rcpp::warning(message);
// }

}

#endif
