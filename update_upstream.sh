#!/bin/sh -x
SOURCES=.odeint-v2
SOURCES_URI=git://github.com/headmyshoulder/odeint-v2
GIT_SOURCES="--git-dir=$SOURCES/.git --work-tree=$SOURCES"
VERSION=v2.4

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
rsync -az $SOURCES/include/boost/ inst/include/boost/

