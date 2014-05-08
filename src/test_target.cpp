#include "target_cpp.hpp"
#include "target_class.hpp"
#include <Rcpp.h> // Rcpp::as, Rcpp::wrap

namespace rodeint {
namespace test {

void harmonic_oscillator_derivs(const std::vector<double>& y,
                                std::vector<double>& dydt,
                                const double /* t */,
                                const std::vector<double>& pars) {
  dydt[0] =  y[1];
  dydt[1] = -y[0] - pars[0] * y[1];
}

class harmonic_oscillator {
public:
  harmonic_oscillator(double p_) : p(p_) {}
  void derivs(const std::vector<double>& y,
              std::vector<double>& dydt,
              const double /* t */) { // not const
    dydt[0] =  y[1];
    dydt[1] = -y[0] - p * y[1];
  }
  void set_pars(SEXP pars) {
    p = Rcpp::as<double>(pars);
  }
  SEXP get_pars() const {
    return Rcpp::wrap(p);
  }
private:
  double p;
};

}
}

// [[Rcpp::export]]
rodeint::target_cpp test_harmonic_oscillator_cpp() {
  rodeint::target_cpp::pars_type pars(1); // establishes required length
  return rodeint::target_cpp(&rodeint::test::harmonic_oscillator_derivs, pars);
}

// [[Rcpp::export]]
rodeint::target_class test_harmonic_oscillator_class(double p) {
  using rodeint::test::harmonic_oscillator;
  harmonic_oscillator obj(p);
  return rodeint::wrapper<harmonic_oscillator>::make_target(obj);
}
