#!/usr/bin/env bash
set -euo pipefail

BRANCH=${1:-master}
IMAGE=kyleondy/website:$BRANCH


make clean && make check && make package
cp _output/$BRANCH.tar.gz ./docker/site.tar.gz
docker build docker -t $IMAGE

if [ $BRANCH == "master" ]
then
  IMAGE=kyleondy/website:production
  docker tag kyleondy/website:master $IMAGE
fi

docker push $IMAGE
