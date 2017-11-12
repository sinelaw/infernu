#!/bin/bash
set -e
set -u
SCRIPT_DIR=$(dirname $(readlink -f $0))
cd $SCRIPT_DIR
ARGS="$@"
#echo $ARGS
>&2 printf "."
((timeout 3 stack exec infernu-demo $ARGS) || echo "ERROR") 2>&1 | tail -1 | cut -b-70 |  xargs -0 -d'\n'  -iresult printf "%-5s %s\n" result "$ARGS"
