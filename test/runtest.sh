#!/bin/bash
set -e
set -u
../dist/build/sjs-demo/sjs-demo $@ | tail -1 | xargs echo $@
