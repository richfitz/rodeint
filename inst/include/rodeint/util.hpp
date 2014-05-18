#ifndef _RODEINT_UTIL_HPP_
#define _RODEINT_UTIL_HPP_

#include <rodeint/common.hpp>
#include <Rcpp.h>
#include <vector>
#include <sstream> // std::stringstream

namespace rodeint {

void check_dt(double t0, double t1, double dt);

namespace util {

// Longer term this should be transparent to the user, but I don't
// think it's worth checking all the time.  Perhaps it is?
template <typename T>
void check_ptr_valid(Rcpp::XPtr<T> p) {
  T* test = p;
  if (test == NULL) {
    util::stop("Pointer is NULL - please run rebuild()");
  }
}

template <typename T>
Rcpp::NumericMatrix
to_rcpp_matrix_by_row(const std::vector<T>& x) {
  const size_t nr = x.size();
  if (nr == 0) {
    util::stop("Can't make matrix of empty vector");
  }
  Rcpp::NumericMatrix ret(static_cast<int>(nr),
                          static_cast<int>(x.begin()->size()));
  for (size_t i = 0; i < nr; ++i) {
    Rcpp::NumericMatrix::Row r = ret(static_cast<int>(i), Rcpp::_);
    std::copy(x[i].begin(), x[i].end(), r.begin());
  }
  return ret;
}

// Given a vector-of-vectors, copy the vector x[i] into the ith
// *column* of an Rcpp matrix.
template <typename T>
Rcpp::NumericMatrix
to_rcpp_matrix_by_col(const std::vector<T>& x) {
  const size_t nc = x.size();
  if (nc == 0) {
    util::stop("Can't make matrix of empty vector");
  }
  Rcpp::NumericMatrix ret(static_cast<int>(x.begin()->size()),
                          static_cast<int>(nc));
  Rcpp::NumericMatrix::iterator it = ret.begin();
  for (size_t i = 0; i < nc; ++i) {
    it = std::copy(x[i].begin(), x[i].end(), it);
  }
  return ret;
}

template<typename T>
std::string to_string(T x) {
  std::ostringstream o;
  if (!(o << x))
    util::stop("String conversion failure");
  return o.str();
}

size_t safe_size_t_from_r(int n);

// Based on
//   http://www.cplusplus.com/reference/algorithm/is_sorted/
// but allows increasing/decreasing checking.
template <class ForwardIterator>
bool is_sorted(ForwardIterator first, ForwardIterator last,
               bool increasing) {
  if (first == last) {
    return true;
  }
  ForwardIterator next = first;
  while (++next!=last) {
    if (*next < *first == increasing) {
      return false;
    }
    ++first;
  }
  return true;
}

inline void check_length(size_t recieved, size_t expected) {
  if (expected != recieved)
    util::stop("Incorrect length input; expected " +
               to_string(expected) + ", recieved " +
               to_string(recieved));
}

}

// TODO: May move elsewhere.
template <typename State>
Rcpp::NumericVector
integration_state(const typename State::state_type& y,
                  const State& state,
                  bool save_state) {
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (save_state) {
    ret.attr("steps") = state.steps;
    ret.attr("t")     = state.t;
    ret.attr("y")     = util::to_rcpp_matrix_by_row(state.y);
  }
  return ret;
}

}

#endif
