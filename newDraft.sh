#!/usr/bin/env bash
TITLE=$1

if [ -z "$TITLE" ]; then
  echo "need draft title"
  exit 1
fi

FILENAME="${TITLE,,}"
# replace spaces with dashed, stip non alphanumberics
FILENAME="$( echo "${FILENAME// /-}" | tr -cd '[[:alnum:]]._-')"

DRAFT="provider/drafts/$FILENAME.md"
if [ -f "$DRAFT" ]; then
  echo "$DRAFT already exisits. Will NOT overwrite"
  exit 1
fi

cat >"$DRAFT" <<EOL
---
title: $TITLE
tags: draft
created: $(date +"%FT%TZ")
author: Kyle Ondy
subtitle: todo, add subtitle
---
EOL
