#!/usr/bin/env sh
set -eu
DRONE_KEY=/tmp/drone_key

if [ $1 == "staging" ]
then
  BRANCH=staging
else
  if [ $1 == "production" ]
  then BRANCH=production
  else
    echo Not a valid branch
    exit 1
  fi
fi

echo Deploying $BRANCH

rm -f $DRONE_KEY
echo "$RSYNC_PRIVATE_KEY" > $DRONE_KEY
chmod 400 $DRONE_KEY
rsync -avP -e"ssh -p 3220 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i /tmp/drone_key" _output/latest.tar.gz root@static.ondy.me:/data/kyleondy.com/$BRANCH.tar.gz
curl -X POST $DOCKER_HUB_HOOK
