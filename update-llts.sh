#!/usr/bin/env bash

# change Stack's llts value in both the travis.ci config,
# and in the stack.yml

if [ -z "$1" ]; then
  echo "Pust pass a value for new lts"
fi

sed -i "s/^resolver:.*/resolver: $1/g" ./stack.yaml
sed -i "s/.*DEPLOY=true$/    - MATRIX_ARGS=--resolver=$1 DEPLOY=true/g" ./.travis.yml
