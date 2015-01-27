#!/bin/bash
set -e
set -u
ARGS="$@"
#echo $ARGS
(../dist/build/inferno-demo/inferno-demo $ARGS   || echo "ERROR") 2>&1 | tail -1 | xargs -iresult printf "%-5s %s\n" result "$ARGS"
