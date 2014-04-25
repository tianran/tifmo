#!/bin/sh
SCALA=scala
CORENLP_VERSION=stanford-corenlp-full-2014-01-04

export JAVA_OPTS=-Xmx2g

PRODUCTION_PATH=target/production
CLASSPATH_EN=$PRODUCTION_PATH:resources:lib/*:lib/en/*:lib/en/$CORENLP_VERSION/*

THISFILE="$0"
exec $SCALA -classpath $CLASSPATH_EN demos/"${THISFILE%.sh}".scala "$@"
