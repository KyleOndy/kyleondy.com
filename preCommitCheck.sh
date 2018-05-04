#!/usr/bin/env bash

# only care about things in the provider dir
CHANGED_FILES=$(git diff --name-only | grep 'provider' | grep -Ev '\.html$|\.scss$')
SECONDS_NOW=$(date +%s)
RETURN_CODE=0

for f in $CHANGED_FILES; do
  if (git diff "${f}" | grep -q '+updated:.*$'); then
    # git diff has a line matching (added)

    # keeping it simple; using cut for now
    UPDATED=$(grep '^updated: ' "${f}" | cut -d " " -f2)
    SECONDS_SINCE=$(date --date="$UPDATED" +%s)
    SECONDS_DIFF=$((SECONDS_NOW-SECONDS_SINCE))
    HOURS_DIFF=$((SECONDS_DIFF / 60 / 60))
    if [ "$HOURS_DIFF" -gt "24" ]; then
      echo "${f} has an 'updated' time ~${HOURS_DIFF} ago"
      RETURN_CODE=1
    fi
  else
    echo "Could not find 'updated:' in the diff for '${f}'."
    RETURN_CODE=1
  fi
done

if [ "$RETURN_CODE" -ne 0 ]; then
  echo "Possible errors. Check before commiting."
  exit 1
fi
echo "All looks good!"
exit 0
