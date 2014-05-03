## This is going to change at some point, but this is a list of the
## possible controlled stepper types.  Once I work out what to do
## about non-controlled steppers (dense output, etc), then this might
## change.
controlled_stepper_types <- function() {
  c("runge_kutta_cash_karp54",
    "runge_kutta_fehlberg78",
    "runge_kutta_dopri5")  
}
