#include <rodeint_ode_system.h>

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
  typedef double pars_type;
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
private:
  double p;
};

}
}

// [[Rcpp::export]]
rodeint::ode_system_cpp
test_harmonic_oscillator_cpp(std::vector<double> pars) {
  using rodeint::test::harmonic_oscillator_derivs;
  return rodeint::ode_system_cpp(&harmonic_oscillator_derivs, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class
test_harmonic_oscillator_class(double pars) {
  using rodeint::test::harmonic_oscillator;
  return rodeint::ode_system_class_generator<harmonic_oscillator>(pars);
}
