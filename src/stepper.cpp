#include <rodeint/stepper.hpp>
#include <Rcpp.h> // Rcpp::stop

namespace rodeint {

// Note: can do names this way too.
//                                     EU,   MM,   RK4,  RKCK, RKF,  RKD,  BS,   R4
const bool stepper::ok_basic[]      = {true, true, true, true, true, true, false,true};
const bool stepper::ok_controlled[] = {false,false,false,true, true, true, true, true};
const bool stepper::ok_dense[]      = {true, false,false,false,false,true, true, true};
const bool stepper::needs_ublas[]   = {false,false,false,false,false,false,false,true};

std::string stepper::category_name(stepper::Category category) {
  std::string ret;
  switch(category) {
  case BASIC:      return "basic";
  case CONTROLLED: return "controlled";
  case DENSE:      return "dense";
  default: stop("Invalid category");
  }
}

std::string stepper::algorithm_name(stepper::Algorithm algorithm) {
  std::string ret;
  switch(algorithm) {
  case EULER:                   return "euler";
  case MODIFIED_MIDPOINT:       return "modified_midpoint";
  case RUNGE_KUTTA4:            return "runge_kutta4";
  case RUNGE_KUTTA_CASH_KARP54: return "runge_kutta_cash_karp54";
  case RUNGE_KUTTA_FEHLBERG78:  return "runge_kutta_fehlberg78";
  case RUNGE_KUTTA_DOPRI5:      return "runge_kutta_dopri5";
  case BULIRSCH_STOER:          return "bulirsch_stoer";
  case ROSENBROCK4:             return "rosenbrock4";
  default: stop("Invalid algorithm");
  }
}

// Inverse direction:
stepper::Category
stepper::category_from_string(const std::string& x) {
  Category ret = BASIC;
  // Please excuse the odd formatting.
  if      (x == "basic")      {ret = BASIC;     }
  else if (x == "controlled") {ret = CONTROLLED;}
  else if (x == "dense")      {ret = DENSE;     }
  else {stop("Invalid category " + x);}
  return ret;
}

stepper::Algorithm stepper::algorithm_from_string(const std::string& x) {
  Algorithm ret = EULER;
  // Please excuse the odd formatting.
  if      (x == "euler")                   {ret = EULER;                  }
  else if (x == "modified_midpoint")       {ret = MODIFIED_MIDPOINT;      }
  else if (x == "runge_kutta4")            {ret = RUNGE_KUTTA4;           }
  else if (x == "runge_kutta_cash_karp54") {ret = RUNGE_KUTTA_CASH_KARP54;}
  else if (x == "runge_kutta_fehlberg78")  {ret = RUNGE_KUTTA_FEHLBERG78; }
  else if (x == "runge_kutta_dopri5")      {ret = RUNGE_KUTTA_DOPRI5;     }
  else if (x == "bulirsch_stoer")          {ret = BULIRSCH_STOER;         }
  else if (x == "rosenbrock4")             {ret = ROSENBROCK4;            }
  else {stop("Invalid algorithm " + x);}
  return ret;
}

void stepper::validate(stepper::Category category,
                       stepper::Algorithm algorithm,
                       bool ublas_state,
                       double abs_tol, double rel_tol) {
  if (category == BASIC) {
    if (!ok_basic[algorithm]) {
      stop("Cannot make a basic stepper with algorithm " +
           algorithm_name(algorithm));
    }
    if (!R_IsNA(abs_tol) || !R_IsNA(rel_tol)) {
      stop("Basic steppers must have NA tolerances");
    }
  } else if (category == CONTROLLED) {
    if (!ok_controlled[algorithm]) {
      stop("Cannot make a controlled stepper with algorithm " +
           algorithm_name(algorithm));
    }
    if (R_IsNA(abs_tol) || R_IsNA(rel_tol)) {
      stop("Tolerances must be non-NA");
    }
  } else if (category == DENSE) {
    if (!ok_dense[algorithm]) {
      stop("Cannot make a dense stepper of algorithm " +
           algorithm_name(algorithm));
    }
    // NOTE: These are ignored by euler though...
    if (R_IsNA(abs_tol) || R_IsNA(rel_tol)) {
      stop("Tolerances must be non-NA");
    }
  }
  if (!ublas_state && needs_ublas[algorithm]) { //
    stop("The stepper algorithm " + algorithm_name(algorithm) +
         " requires a uBLAS state");
  }
}

boost::any stepper::construct(stepper::Category category,
                              stepper::Algorithm algorithm,
                              bool stiff,
                              double abs_tol, double rel_tol) {
  validate(category, algorithm, stiff, abs_tol, rel_tol);
  if (stiff) {
    return construct<vector_ublas>(category, algorithm, abs_tol, rel_tol);
  } else {
    return construct<vector_stl>(category, algorithm, abs_tol, rel_tol);
  }
}

// OK, lots of duplication, but we can code generate it away.
template <>
boost::any
stepper::construct_basic<vector_stl>(stepper::Algorithm algorithm) {
  switch(algorithm) {
  case EULER:
    return stepper_basic_euler_stl();
  case MODIFIED_MIDPOINT:
    return stepper_basic_modified_midpoint_stl();
  case RUNGE_KUTTA4:
    return stepper_basic_runge_kutta4_stl();
  case RUNGE_KUTTA_CASH_KARP54:
    return stepper_basic_runge_kutta_cash_karp54_stl();
  case RUNGE_KUTTA_FEHLBERG78:
    return stepper_basic_runge_kutta_fehlberg78_stl();
  case RUNGE_KUTTA_DOPRI5:
    return stepper_basic_runge_kutta_dopri5_stl();
  default:
    stop("Invalid basic algorithm"); // TODO: print algorithm
    return boost::any();
  }
}

template <>
boost::any
stepper::construct_basic<vector_ublas>(stepper::Algorithm algorithm) {
  switch(algorithm) {
  case EULER:
    return stepper_basic_euler_ublas();
  case MODIFIED_MIDPOINT:
    return stepper_basic_modified_midpoint_ublas();
  case RUNGE_KUTTA4:
    return stepper_basic_runge_kutta4_ublas();
  case RUNGE_KUTTA_CASH_KARP54:
    return stepper_basic_runge_kutta_cash_karp54_ublas();
  case RUNGE_KUTTA_FEHLBERG78:
    return stepper_basic_runge_kutta_fehlberg78_ublas();
  case RUNGE_KUTTA_DOPRI5:
    return stepper_basic_runge_kutta_dopri5_ublas();
  case ROSENBROCK4:
    return stepper_basic_rosenbrock4_ublas();
  default:
    stop("Invalid basic algorithm"); // TODO: print algorithm
    return boost::any();
  }
}

template <>
boost::any
stepper::construct_controlled<vector_stl>(stepper::Algorithm algorithm,
                                          double abs_tol,
                                          double rel_tol) {
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::runge_kutta_cash_karp54;
  using boost::numeric::odeint::runge_kutta_fehlberg78;
  using boost::numeric::odeint::runge_kutta_dopri5;
  using boost::numeric::odeint::bulirsch_stoer;
  typedef rodeint::vector_stl state;

  switch(algorithm) {
  case RUNGE_KUTTA_CASH_KARP54:
    return make_controlled<runge_kutta_cash_karp54<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_FEHLBERG78:
    return make_controlled<runge_kutta_fehlberg78<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_DOPRI5:
    return make_controlled<runge_kutta_dopri5<state> >(abs_tol, rel_tol);
  case BULIRSCH_STOER:
    return bulirsch_stoer<state>(abs_tol, rel_tol);
  default:
    stop("Invalid controlled algorithm"); // TODO: print algorithm
    return boost::any();
  }
}

template <>
boost::any
stepper::construct_controlled<vector_ublas>(stepper::Algorithm algorithm,
                                            double abs_tol,
                                            double rel_tol) {
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::runge_kutta_cash_karp54;
  using boost::numeric::odeint::runge_kutta_fehlberg78;
  using boost::numeric::odeint::runge_kutta_dopri5;
  using boost::numeric::odeint::bulirsch_stoer;
  typedef rodeint::vector_ublas state;

  switch(algorithm) {
  case RUNGE_KUTTA_CASH_KARP54:
    return make_controlled<runge_kutta_cash_karp54<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_FEHLBERG78:
    return make_controlled<runge_kutta_fehlberg78<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_DOPRI5:
    return make_controlled<runge_kutta_dopri5<state> >(abs_tol, rel_tol);
  case BULIRSCH_STOER:
    return bulirsch_stoer<state>(abs_tol, rel_tol);
  case ROSENBROCK4:
    return make_controlled<stepper_basic_rosenbrock4_ublas>(abs_tol, rel_tol);
  default:
    stop("Invalid controlled algorithm"); // TODO: print algorithm
    return boost::any();
  }
}

template <>
boost::any
stepper::construct_dense<vector_stl>(stepper::Algorithm algorithm,
                                     double abs_tol,
                                     double rel_tol) {
  using boost::numeric::odeint::make_dense_output;
  using boost::numeric::odeint::bulirsch_stoer_dense_out;

  switch(algorithm) {
  case EULER:
    return stepper_dense_euler_stl();
  case RUNGE_KUTTA_DOPRI5:
    return
      make_dense_output<stepper_basic_runge_kutta_dopri5_stl>
      (abs_tol, rel_tol);
  case BULIRSCH_STOER:
    return bulirsch_stoer_dense_out<vector_stl>(abs_tol, rel_tol);
  default:
    stop("Invalid controlled algorithm"); // TODO: print algorithm
    return boost::any();
  }
}

template <>
boost::any
stepper::construct_dense<vector_ublas>(stepper::Algorithm algorithm,
                                       double abs_tol,
                                       double rel_tol) {
  using boost::numeric::odeint::make_dense_output;
  using boost::numeric::odeint::bulirsch_stoer_dense_out;
  using boost::numeric::odeint::rosenbrock4_dense_output;

  switch(algorithm) {
  case EULER:
    return stepper_dense_euler_ublas();
  case RUNGE_KUTTA_DOPRI5:
    make_dense_output<stepper_basic_runge_kutta_dopri5_ublas>
      (abs_tol, rel_tol);
  case BULIRSCH_STOER:
    return bulirsch_stoer_dense_out<vector_ublas>(abs_tol, rel_tol);
  case ROSENBROCK4:
    return make_dense_output<stepper_basic_rosenbrock4_ublas
                             >(abs_tol, rel_tol);
  default:
    stop("Invalid controlled algorithm"); // TODO: print algorithm
    return boost::any();
  }
}

} // namespace

// [[Rcpp::export]]
rodeint::stepper
stepper__ctor(std::string category, std::string algorithm, bool ublas_state,
              double abs_tol, double rel_tol) {
  return rodeint::stepper(category, algorithm, ublas_state, abs_tol, rel_tol);
}

// [[Rcpp::export]]
Rcpp::CharacterVector stepper__details(rodeint::stepper s) {
  Rcpp::CharacterVector ret;
  ret.push_back(s.category_name());
  ret.push_back(s.algorithm_name());
  ret.attr("category_id")    = static_cast<int>(s.category_id());
  ret.attr("algorithm_id")   = static_cast<int>(s.algorithm_id());
  ret.attr("ublas_state")    = s.has_ublas_state();
  ret.attr("needs_jacobian") = s.needs_jacobian();
  return ret;
}
