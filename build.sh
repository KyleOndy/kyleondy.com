#!/usr/bin/env bash
set -e

_log() {
  echo "==> $*"
}

LTS=$(yq r stack.yaml resolver)
REV=$(git rev-parse HEAD)

docker build --build-arg "LTS=$LTS" -t kyleondy-web:latest .
docker tag kyleondy-web:latest "registry.apps.509ely.com/kyleondy-web:$REV"


# if deploy
_log "pusing $REV"
docker push "registry.apps.509ely.com/kyleondy-web:$REV" | cat
docker tag "registry.apps.509ely.com/kyleondy-web:$REV" "registry.apps.509ely.com/kyleondy-web:latest"
_log "pusing latest"
docker push "registry.apps.509ely.com/kyleondy-web:latest"

if [[ "$1" == "prod" ]]; then
  # if prod
  docker tag "registry.apps.509ely.com/kyleondy-web:$REV" "registry.apps.509ely.com/kyleondy-web:prod"
  _log "pusing prod"
  docker push "registry.apps.509ely.com/kyleondy-web:prod"
fi
