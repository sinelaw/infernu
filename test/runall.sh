#!/bin/bash
set -e
set -u
find valid -type f -iname "*.js"| sort |xargs -n1 -t ./runtest.sh y
find invalid -type f -iname "*.js" | sort|xargs -n1 -t ./runtest.sh n
