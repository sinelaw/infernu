#!/bin/bash
set -e
set -u
find valid -type f -iname "*.js"| sort |xargs -n1  ./runtest.sh y
find invalid -type f -iname "*.js" | sort|xargs -n1  ./runtest.sh n
>&2 echo ""

