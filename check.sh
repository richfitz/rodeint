#!/bin/sh
RETVAL=0

SIZE_T=$(grep size_t src/RcppExports.cpp)
if test $? = 0; then
    echo "*** Found potentially dangerous size_t in exported functions:"
    echo
    echo "$SIZE_T"
    echo
    RETVAL=1
fi

exit $RETVAL
