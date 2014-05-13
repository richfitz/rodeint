#ifndef _RODEINT_OBSERVERS_HPP_
#define _RODEINT_OBSERVERS_HPP_

#include <vector>

namespace rodeint {

// NOTE: This is probably the only observer that will be added until
// the stiff systems are supported.  Then, at the least, we need to
// support a different state type.
template <typename T>
struct state_saver {
  struct observer {
    // Note this is a vector of state types.
    std::vector<T>&      y;
    std::vector<double>& t;
    observer(std::vector<T>& y_, std::vector<double>& t_) : y(y_), t(t_) {}
    void operator()(const T &yi, double ti) {
      y.push_back(yi);
      t.push_back(ti);
    }
  };
  state_saver() : steps(0), obs(y, t) {}
  std::vector<T>      y;
  std::vector<double> t;
  size_t steps;
  observer obs;
};

}

#endif
