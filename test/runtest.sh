#!/bin/bash
set -e
set -u
../dist/build/sjs-demo/sjs-demo $@ 2>&1 | tail -1 | xargs echo $@
