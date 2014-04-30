#!/bin/sh -x
SOURCES=.odeint-v2
SOURCES_URI=git://github.com/headmyshoulder/odeint-v2
GIT_SOURCES="--git-dir=$SOURCES/.git --work-tree=$SOURCES"
# There is a change in structure at v2.4 where include files change
# location
SOURCES_INCLUDE_PRE_2_4=$SOURCES/boost/
SOURCES_INCLUDE_POST_2_4=$SOURCES/include/boost/
SOURCES_EXAMPLES_PRE_2_4=$SOURCES/libs/numeric/odeint/examples
SOURCES_EXAMPLES_POST_2_4=$SOURCES/examples

VERSION=v2.2
SOURCES_INCLUDE=$SOURCES_INCLUDE_PRE_2_4
SOURCES_EXAMPLES=$SOURCES_EXAMPLES_PRE_2_4

if test -d $SOURCES
then
    git $GIT_SOURCES checkout master
    git $GIT_SOURCES fetch
else
    git clone $SOURCES_URI $SOURCES
    git $GIT_SOURCES branch tracking
fi

# Ensure that HEAD is at the version that we want to track
git $GIT_SOURCES checkout tracking
git $GIT_SOURCES reset --hard --quiet $VERSION

mkdir -p inst/include
rsync -az $SOURCES_INCLUDE inst/include/boost/
# And the examples:
cp $SOURCES_EXAMPLES/lorenz_point.cpp examples
