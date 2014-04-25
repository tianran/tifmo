#!/bin/sh
SCALA=scala
THISFILE="$0"
exec $SCALA demos/"${THISFILE%.sh}".scala "$@"
