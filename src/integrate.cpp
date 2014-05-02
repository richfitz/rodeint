#include "integrate.hpp"
#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"
#include "observers.hpp"
#include "controlled_stepper.hpp"
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

// In theory, non-controlled steppers could be used here.  We could
// generalise out even more of the hassle if boost variant would work
// in a nested way there (it might).
//
// Some serious weirdness going on with attributes here:
//
// integrate_adaptive does not work as a name, but
// do_integrate_adapative,a nd controlled_stepper__integrate_adaptive
// does.
//
// [[Rcpp::export]]
Rcpp::NumericVector
controlled_stepper__integrate_adaptive(rodeint::controlled_stepper stepper,
                                       rodeint::target_r target,
                                       rodeint::target_r::state_type y,
                                       double t0, double t1,
                                       double dt,
                                       bool with_info=false) {
  rodeint::controlled_stepper_integrate_adaptive_visitor
    vis(target, y, t0, t1, dt);
  boost::apply_visitor(vis, stepper);
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (with_info) {
    ret.attr("steps") = vis.last_steps();
  }
  return ret;
}
