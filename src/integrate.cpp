#include "integrate.hpp"
#include <boost/numeric/odeint.hpp>
#include "target_r.hpp"
#include "observers.hpp"
#include "controlled_stepper.hpp"
#include "util.hpp"

// [[Rcpp::export(integrate)]]
Rcpp::NumericVector
r_integrate(rodeint::target_r target,
            rodeint::target_r::state_type y, 
            double t0, double t1, double dt,
            bool save_state=false) {
  typedef rodeint::target_r::state_type state_type;
  using boost::numeric::odeint::integrate;

  std::vector<state_type> y_vec;
  std::vector<double> t_vec;
  rodeint::obs_save_state<state_type> obs(y_vec, t_vec);
  size_t n_steps = 0;
  
  if (save_state) {
    n_steps = integrate(target, y, t0, t1, dt, obs);
  } else {
    integrate(target, y, t0, t1, dt);
  }

  Rcpp::NumericVector ret(y.begin(), y.end());

  if (save_state) {
    ret.attr("steps") = n_steps;
    ret.attr("t")     = t_vec;
    ret.attr("y")     = rodeint::to_rcpp_matrix_by_row(y_vec);
  }

  return ret;
}

// In theory, non-controlled steppers could be used here.  We could
// generalise out even more of the hassle if boost variant would work
// in a nested way there (it might).
//
// [[Rcpp::export(integrate_adaptive)]]
Rcpp::NumericVector
r_integrate_adaptive(rodeint::controlled_stepper stepper,
                     rodeint::target_r target,
                     rodeint::target_r::state_type y,
                     double t0, double t1, double dt,
                     bool save_state=false) {
  rodeint::controlled_stepper_integrate_adaptive
    vis(target, y, t0, t1, dt, save_state);
  boost::apply_visitor(vis, stepper);
  Rcpp::NumericVector ret(y.begin(), y.end());
  if (save_state) {
    ret.attr("steps") = vis.steps;
    ret.attr("t")     = vis.t_vec;
    ret.attr("y")     = rodeint::to_rcpp_matrix_by_row(vis.y_vec);
  }
  return ret;
}
