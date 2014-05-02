#include "integrate.hpp"
#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"
#include "observers.hpp"
#include "util.hpp"

// [[Rcpp::export]]
rodeint::target_r::state_type
integrate(rodeint::target_r target,
          rodeint::target_r::state_type y, 
          double t0, double t1,
          double dt) {
  boost::numeric::odeint::integrate(target, y, t0, t1, dt);
  return y;
}

// [[Rcpp::export]]
Rcpp::List integrate_observed(rodeint::target_r target,
                              rodeint::target_r::state_type y, 
                              double t0, double t1,
                              double dt) {
  typedef rodeint::target_r::state_type state_type;
  // Perhaps store these as an attribute, rather than return the list?
  std::vector<state_type> y_vec;
  std::vector<double> t_vec;
  rodeint::obs_save_state<state_type> obs(y_vec, t_vec);

  boost::numeric::odeint::integrate(target, y, t0, t1, dt, obs);
  
  return Rcpp::List::create(Rcpp::_["t"]=Rcpp::wrap(t_vec),
                            Rcpp::_["y"]=to_rcpp_matrix_by_row(y_vec));
}
