#include "target_cpp.hpp"

namespace rodeint {
namespace test {

void harmonic_oscillator_derivs(const std::vector<double>& y,
                                std::vector<double>& dydt,
                                const double /* t */,
                                const std::vector<double>& pars) {
  dydt[0] =  y[1];
  dydt[1] = -y[0] - pars[0] * y[1];
}

}
}

// [[Rcpp::export]]
rodeint::target_cpp test_harmonic_oscillator() {
  rodeint::target_cpp::pars_type pars(1); // establishes required length
  return rodeint::target_cpp(&rodeint::test::harmonic_oscillator_derivs, pars);
}
