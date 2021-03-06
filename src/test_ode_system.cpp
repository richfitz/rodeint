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
              const double /* t */) { // method not const
    dydt[0] =  y[1];
    dydt[1] = -y[0] - p * y[1];
  }
  void set_pars(SEXP pars) {
    p = Rcpp::as<double>(pars);
  }
private:
  double p;
};

// Same system in deSolve style -- here is the system.
static double test_harmonic_oscillator_parms[1];
void harmonic_oscillator_desolve_func(int*    /* neq  */,
                                      double* /* t    */,
                                      double*    y,
                                      double*    dydt,
                                      double* /* yout */,
                                      int*    /* ip   */) {
  double p = test_harmonic_oscillator_parms[0];
  dydt[0] =  y[1];
  dydt[1] = -y[0] - p * y[1];
}

// Stiff system
typedef rodeint::ode_system_stiff_cpp::state_type  stiff_vector;
typedef rodeint::ode_system_stiff_cpp::matrix_type stiff_matrix;
typedef std::vector<double> pars_vector;
void stiff_derivs(const stiff_vector& y, stiff_vector& dydt,
                  const double /* t */,
                  const pars_vector& /* pars */) {
  dydt[0] = -101.0 * y[0] - 100.0 * y[1];
  dydt[1] = y[0];
}

void stiff_jacobian(const stiff_vector&, stiff_matrix& J,
                    const double /* t */, stiff_vector& dfdt,
                    const pars_vector& /* pars */) {
  J(0, 0) = -101.0;
  J(0, 1) = -100.0;
  J(1, 0) = 1.0;
  J(1, 1) = 0.0;
  dfdt[0] = 0.0;
  dfdt[1] = 0.0;
}

class stiff_system {
public:
  typedef std::vector<double> pars_type; // still needs declaring
  typedef rodeint::ode_system_stiff_class::state_type  vec_type;
  typedef rodeint::ode_system_stiff_class::matrix_type mat_type;
  stiff_system(std::vector<double> /* pars */) {} // ignore parameters
  void derivs(const vec_type& y, vec_type& dydt, const double /* t */) {
    dydt[0] = -101.0 * y[0] - 100.0 * y[1];
    dydt[1] = y[0];
  }
  void jacobian(const vec_type& /* y */, mat_type& J, const double /* t */,
                vec_type& dfdt) {
    J(0, 0) = -101.0;
    J(0, 1) = -100.0;
    J(1, 0) = 1.0;
    J(1, 1) = 0.0;
    dfdt[0] = 0.0;
    dfdt[1] = 0.0;
  }
  void set_pars(SEXP /* pars */) { } // noop
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
  return rodeint::make_ode_system_class<harmonic_oscillator>(pars);
}

// [[Rcpp::export]]
rodeint::ode_system_class
test_harmonic_oscillator_deSolve_c(std::vector<double> pars) {
  using namespace rodeint::deSolve;
  using namespace rodeint::test;
  func_type* func = &harmonic_oscillator_desolve_func;
  double* pars_deSolve = test_harmonic_oscillator_parms;
  return make_ode_system_deSolve(func, pars_deSolve, pars);
}

// [[Rcpp::export]]
rodeint::ode_system_stiff_cpp
test_stiff_cpp(std::vector<double> pars) {
  using rodeint::test::stiff_derivs;
  using rodeint::test::stiff_jacobian;
  return rodeint::ode_system_stiff_cpp(&stiff_derivs,
                                       &stiff_jacobian,
                                       pars);
}

// [[Rcpp::export]]
rodeint::ode_system_stiff_class
test_stiff_class(std::vector<double> pars) {
  using rodeint::test::stiff_system;
  return rodeint::make_ode_system_stiff_class<stiff_system>(pars);
}
