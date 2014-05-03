#ifndef _RODEINT_OBSERVERS_HPP_
#define _RODEINT_OBSERVERS_HPP_

#include <vector>

namespace rodeint {

// Based on harmonic_oscillator.hpp -- we need to take a reference
// here.

// NOTE: Given that this is the *only* observer that we're likely to
// produce it might be worth renaming this file a bit.  However, later
// on getting other observers might be worthwhile, even if they're not
// actually used in the interface.  Or we could add other features to
// this one such as optional printing.
template <typename T>
struct obs_save_state {
  typedef T state_type;

  std::vector<state_type>& y_vec;
  std::vector<double>&     t_vec;

  // Default state is empty.
  obs_save_state(std::vector<state_type>& y_vec_, 
                 std::vector<double>& t_vec_)
    : y_vec(y_vec_), t_vec(t_vec_) {}

  void operator()(const state_type& y, double t) {
    y_vec.push_back(y);
    t_vec.push_back(t);
  }
};

}

#endif
