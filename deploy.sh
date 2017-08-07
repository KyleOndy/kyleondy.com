#!/usr/bin/env bash
set -euo pipefail

BRANCH=${1:-master}
cp _output/$BRANCH.tar.gz ./docker/site.tar.gz
docker build docker/ -t kyleondy/website:$BRANCH
