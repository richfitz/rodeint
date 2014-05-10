// -*-c++-*-
#ifndef _RODEINT_TARGET_H_
#define _RODEINT_TARGET_H_

#ifdef RODEINT_TARGET_ONLY
#include "rodeint.h"
#else
// This little dance ensures we don't leave a macro defined that would
// make incuding rodeint.h confusing later on.
#define RODEINT_TARGET_ONLY
#include "rodeint.h"
#undef RODEINT_TARGET_ONLY
#endif

#endif
