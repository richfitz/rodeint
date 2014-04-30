#include <Rcpp.h>
#include <boost/numeric/odeint.hpp>
#include "harmonic_oscillator.hpp"

// An observer that stores state and time:
// struct push_back_state_and_time {
//   typedef harmonic_oscillator::state_type state_type;
//   std::vector<state_type>& m_states;
//   std::vector<double>& m_times;

//   push_back_state_and_time(std::vector<state_type> &states,
//                            std::vector<double> &times)
//     : m_states(states), m_times(times) { }

//   void operator()(const state_type &y, double t) {
//     m_states.push_back(y);
//     m_times.push_back(t);
//   }
// };

// Observer to write state:
// struct write_state {
//   void operator()(const harmonic_oscillator::state_type &y) const {
//     std::cout << y[0] << "\t" << y[1] << "\n";
//   }
// };

//' @export
// [[Rcpp::export]]
std::vector<double> harmonic_oscillator_basic(std::vector<double> y, 
                                              double t0, double t1,
                                              double par, double dt) {
  harmonic_oscillator ho(par);
  size_t steps = boost::numeric::odeint::integrate(ho, y, t0, t1, dt);
  std::cout << steps << std::endl;
  return y;
}
