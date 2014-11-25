#!/bin/bash
set -e
set -u
ARGS="$@"
../dist/build/sjs-demo/sjs-demo $ARGS 2>&1 | tail -1 | xargs -iresult printf "%-5s %s\n" result "$ARGS"
