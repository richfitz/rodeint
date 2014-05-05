#ifndef _RODEINT_STEPPER_HPP_
#define _RODEINT_STEPPER_HPP_

#include "stepper_basic.hpp"
#include "stepper_controlled.hpp"
#include <boost/variant.hpp>

namespace rodeint {

typedef
boost::variant<stepper_basic, stepper_controlled> stepper;

class stepper_type_visitor : boost::static_visitor<> {
public:
  typedef std::vector<std::string> result_type;
  result_type operator()(const stepper_basic& s) const {
    const std::string subtype =
      boost::apply_visitor(stepper_basic_type_visitor(), s);
    return join_types("basic", subtype);
  }
  result_type operator()(const stepper_controlled& s) const {
    const std::string subtype =
      boost::apply_visitor(stepper_controlled_type_visitor(), s);
    return join_types("controlled", subtype);
  }
private:
  static std::vector<std::string> join_types(const std::string& type,
                                             const std::string& subtype) {
    std::vector<std::string> ret;
    ret.push_back(subtype);
    ret.push_back(type);
    return ret;
  }
};

}

#endif
