#!/bin/bash -eu
cd $(dirname $0)
mkdir -p images
ls -1 *.dot  | xargs -n1 -i{} dot {} -T png -o images/{}.png
