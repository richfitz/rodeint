import cog
from string import Template

def tp(template, d):
    return Template(template).substitute(d)

def dappend(d, key, val):
    d = d.copy()
    d.update({key: val, key.upper(): val.upper()})
    return d

## This is going to be common to so many things:
system_types = ['r', 'cpp', 'class']
categories = ['basic', 'controlled']
types_nonstiff = {'basic': ['euler',
                            'modified_midpoint',
                            'runge_kutta4',
                            'runge_kutta_cash_karp54',
                            'runge_kutta_fehlberg78',
                            'runge_kutta_dopri5'],
                  'controlled': ['runge_kutta_cash_karp54',
                                 'runge_kutta_fehlberg78',
                                 'runge_kutta_dopri5']}
types_stiff = {'basic': types_nonstiff['basic'] + ['rosenbrock4'],
               'controlled': types_nonstiff['controlled'] + ['rosenbrock4']}

header = '// *** Generated section: do not edit until the end marker'

# Need to switch return type based on if this is integrate_times or
# not, or just pass it in directly.
def integrate(return_type, types, storage):
    method = """
${return_type} run() {
  switch(s.category_id()) {${switch}
  default:
    stop("Unimplemented category"); // TODO: give details
    return ${return_type}(); // never get here
  }
}"""
    switch = """
  case stepper::${CATEGORY}:
    return run_${category}();"""
    cog.outl(header)
    d = {'return_type': return_type, 'switch': ''}
    for c in categories:
        d['switch'] += tp(switch, dappend(d, 'category', c))
    cog.out(tp(method, d))
    for c in categories:
        integrate_types(return_type, c, types, storage)

def integrate_types(return_type, category, types, storage):
    method = """
${return_type} run_${category}() {
  switch(s.type_id()) {${switch}
  default:
    stop("Unimplemented type"); // TODO: give details
  }
  return ${return_type}(); // never get here
}"""
    switch = """
  case stepper::${TYPE}:
    return run<stepper_${category}_${type}_${storage}>();"""
    d = {'return_type': return_type, 'switch': '',
         'category': category, 'storage': storage}
    for t in types[category]:
        d['switch'] += tp(switch, dappend(d, 'type', t))
    cog.out(tp(method, d))

def integrate_nonstiff(return_type):
    integrate(return_type, types_nonstiff, 'stl')

def integrate_stiff(return_type):
    integrate(return_type, types_stiff, 'ublas')
