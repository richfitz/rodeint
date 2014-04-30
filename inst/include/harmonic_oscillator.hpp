#ifndef _RODEINT_HARMONIC_OSCILLATOR_HPP_
#define _RODEINT_HARMONIC_OSCILLATOR_HPP_

class harmonic_oscillator {
public:
  typedef std::vector<double> state_type;
  harmonic_oscillator(double gam) : m_gam(gam) { }
  void operator()(const state_type &y, state_type &dydt,
                  const double /* t */ ) {
    dydt[0] = y[1];
    dydt[1] = -y[0] - m_gam * y[1];
  }
private:
  double m_gam;
};


#endif
