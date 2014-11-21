#!/bin/bash
set -e
set -u
find valid -type f | xargs -n1 ./runtest.sh y
find invalid -type f | xargs -n1 ./runtest.sh n
