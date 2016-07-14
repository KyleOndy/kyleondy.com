#!/usr/bin/env bash
set -eu
DRONE_KEY=/tmp/drone_key

rm -f $DRONE_KEY
echo "$RSYNC_PRIVATE_KEY" > $DRONE_KEY
chmod 400 $DRONE_KEY
rsync -avP -e"ssh -p 3220 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i /tmp/drone_key" _output/latest.tar.gz root@static.ondy.me:/data/kyleondy.com/
