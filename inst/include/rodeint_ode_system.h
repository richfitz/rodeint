// -*-c++-*-
#ifndef _RODEINT_ODE_SYSTEM_H_
#define _RODEINT_ODE_SYSTEM_H_

// This file *cannot* be included before rodeint.h because the second
// time through rodeint.h will be a noop because of the header
// guards.

// Note that this leaves the RODEINT_ODE_SYSTEM_ONLY macro defined, which
// is not a big deal because of the above.
#define RODEINT_ODE_SYSTEM_ONLY
#include <rodeint.h>

#endif
