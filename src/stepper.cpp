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

std::string stepper::type_name(stepper::Type type) {
  std::string ret;
  switch(type) {
  case EULER:                   return "euler";
  case MODIFIED_MIDPOINT:       return "modified_midpoint";
  case RUNGE_KUTTA4:            return "runge_kutta4";
  case RUNGE_KUTTA_CASH_KARP54: return "runge_kutta_cash_karp54";
  case RUNGE_KUTTA_FEHLBERG78:  return "runge_kutta_fehlberg78";
  case RUNGE_KUTTA_DOPRI5:      return "runge_kutta_dopri5";
  case BULIRSCH_STOER:          return "bulirsch_stoer";
  case ROSENBROCK4:             return "rosenbrock4";
  default: stop("Invalid type");
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

stepper::Type stepper::type_from_string(const std::string& x) {
  Type ret = EULER;
  // Please excuse the odd formatting.
  if      (x == "euler")                   {ret = EULER;                  }
  else if (x == "modified_midpoint")       {ret = MODIFIED_MIDPOINT;      }
  else if (x == "runge_kutta4")            {ret = RUNGE_KUTTA4;           }
  else if (x == "runge_kutta_cash_karp54") {ret = RUNGE_KUTTA_CASH_KARP54;}
  else if (x == "runge_kutta_fehlberg78")  {ret = RUNGE_KUTTA_FEHLBERG78; }
  else if (x == "runge_kutta_dopri5")      {ret = RUNGE_KUTTA_DOPRI5;     }
  else if (x == "bulirsch_stoer")          {ret = BULIRSCH_STOER;         }
  else if (x == "rosenbrock4")             {ret = ROSENBROCK4;            }
  else {stop("Invalid type " + x);}
  return ret;
}

void stepper::validate(stepper::Category category,
                       stepper::Type type,
                       bool stiff_state,
                       double abs_tol, double rel_tol) {
  if (category == BASIC) {
    if (!ok_basic[type]) {
      stop("Cannot make a basic stepper of type " + type_name(type));
    }
    if (!R_IsNA(abs_tol) || !R_IsNA(rel_tol)) {
      stop("Basic steppers must have NA tolerances");
    }
  } else if (category == CONTROLLED) {
    if (!ok_controlled[type]) {
      stop("Cannot make a controlled stepper of type " + type_name(type));
    }
    if (R_IsNA(abs_tol) || R_IsNA(rel_tol)) {
      stop("Tolerances must be non-NA");
    }
  } else if (category == DENSE) {
    if (!ok_dense[type]) {
      stop("Cannot make a dense stepper of type " + type_name(type));
    }
  }
  if (!stiff_state && needs_ublas[type]) { //
    stop("The stepper type " + type_name(type) + " requires a uBLAS state");
  }
}

boost::any stepper::construct(stepper::Category category,
                              stepper::Type type,
                              bool stiff,
                              double abs_tol, double rel_tol) {
  validate(category, type, stiff, abs_tol, rel_tol);
  if (stiff) {
    return construct<stepper_state_type_ublas>
      (category, type, abs_tol, rel_tol);
  } else {
    return construct<stepper_state_type_stl>
      (category, type, abs_tol, rel_tol);
  }
}

// OK, lots of duplication, but we can code generate it away.
template <>
boost::any
stepper::construct_basic<stepper_state_type_stl>(stepper::Type type) {
  switch(type) {
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
    stop("Invalid basic type"); // TODO: print type
  }
  return boost::any();
}

template <>
boost::any
stepper::construct_basic<stepper_state_type_ublas>(stepper::Type type) {
  switch(type) {
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
    stop("Invalid basic type"); // TODO: print type
  }
  return boost::any();
}

template <>
boost::any
stepper::construct_controlled<stepper_state_type_stl>(stepper::Type type,
                                                      double abs_tol,
                                                      double rel_tol) {
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::runge_kutta_cash_karp54;
  using boost::numeric::odeint::runge_kutta_fehlberg78;
  using boost::numeric::odeint::runge_kutta_dopri5;
  typedef rodeint::stepper_state_type_stl state;

  switch(type) {
  case RUNGE_KUTTA_CASH_KARP54:
    return make_controlled<runge_kutta_cash_karp54<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_FEHLBERG78:
    return make_controlled<runge_kutta_fehlberg78<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_DOPRI5:
    return make_controlled<runge_kutta_dopri5<state> >(abs_tol, rel_tol);
    // TODO: bulirsch_stoer
  default:
    stop("Invalid controlled type"); // TODO: print type
  }
  return boost::any();
}

template <>
boost::any
stepper::construct_controlled<stepper_state_type_ublas>(stepper::Type type,
                                                      double abs_tol,
                                                      double rel_tol) {
  using boost::numeric::odeint::make_controlled;
  using boost::numeric::odeint::runge_kutta_cash_karp54;
  using boost::numeric::odeint::runge_kutta_fehlberg78;
  using boost::numeric::odeint::runge_kutta_dopri5;
  typedef rodeint::stepper_state_type_ublas state;

  switch(type) {
  case RUNGE_KUTTA_CASH_KARP54:
    return make_controlled<runge_kutta_cash_karp54<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_FEHLBERG78:
    return make_controlled<runge_kutta_fehlberg78<state> >(abs_tol, rel_tol);
  case RUNGE_KUTTA_DOPRI5:
    return make_controlled<runge_kutta_dopri5<state> >(abs_tol, rel_tol);
    // TODO: bulirsch_stoer
  case ROSENBROCK4:
    return make_controlled<stepper_basic_rosenbrock4_ublas>(abs_tol, rel_tol);
  default:
    stop("Invalid controlled type"); // TODO: print type
  }
  return boost::any();
}

} // namespace

// [[Rcpp::export]]
rodeint::stepper
stepper__ctor(std::string category, std::string type, bool stiff_state,
              double abs_tol, double rel_tol) {
  return rodeint::stepper(category, type, stiff_state, abs_tol, rel_tol);
}

// [[Rcpp::export]]
Rcpp::CharacterVector stepper__type(rodeint::stepper s) {
  Rcpp::CharacterVector ret;
  ret.push_back(s.category_name());
  ret.push_back(s.type_name());
  ret.attr("category_id")    = static_cast<int>(s.category_id());
  ret.attr("type_id")        = static_cast<int>(s.type_id());
  ret.attr("stiff_state")    = s.has_stiff_state();
  ret.attr("needs_jacobian") = s.needs_jacobian();
  return ret;
}
