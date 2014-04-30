#ifndef _RODEINT_POINTER_HPP_
#define _RODEINT_POINTER_HPP_

namespace rodeint {

struct foo {
  foo(double a_, double b_) : a(a_), b(b_) {}
  double run() const {return a+b;}
  double a, b;
};

}

#endif
