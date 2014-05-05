#ifndef _RODEINT_OBSERVERS_HPP_
#define _RODEINT_OBSERVERS_HPP_

#include <vector>
#include <Rcpp.h>
#include "util.hpp"

namespace rodeint {

// Based on harmonic_oscillator.hpp -- we need to take a reference
// here.

// NOTE: Given that this is the *only* observer that we're likely to
// produce it might be worth renaming this file a bit.  However, later
// on getting other observers might be worthwhile, even if they're not
// actually used in the interface.  Or we could add other features to
// this one such as optional printing.
template <typename T>
struct state_saver {
  struct observer {
    std::vector<T>&      y;
    std::vector<double>& t;
    observer(std::vector<T>& y_, std::vector<double>& t_) : y(y_), t(t_) {}
    void operator()(const T &yi, double ti) {
      y.push_back(yi);
      t.push_back(ti);
    }
  };
  state_saver() : steps(0), obs(y, t) {}
  void add_state(Rcpp::NumericVector& ry) const {
    ry.attr("steps") = steps;
    ry.attr("t")     = t;
    ry.attr("y")     = to_rcpp_matrix_by_row(y);
  }
  std::vector<T>      y;
  std::vector<double> t;
  size_t steps;
  observer obs;
};

}

#endif
