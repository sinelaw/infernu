#!/bin/bash
set -e
set -u
find valid -type f -iname "*.js" -print0 | xargs -0 -n1  ./runtest.sh y | sort
find invalid -type f -iname "*.js" -print0 | xargs -0 -n1  ./runtest.sh n | sort
>&2 echo ""

