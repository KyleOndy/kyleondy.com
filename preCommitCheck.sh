#!/usr/bin/env bash

return_code=0

function _pretty_time () {
  if [ -z "$1" ]; then
    echo "Need a time in seconds to print"
    exit 1
  fi
  _seconds=$1
  if [ "$_seconds" -lt 60 ]; then
    echo "less than a minute"
    exit 0
  fi
  _minutes=$(( _seconds / 60 ))
  if [ $_minutes -lt 60 ]; then
    echo "$_minutes minutes"
    exit 0
  fi
  _hours=$(( _minutes / 60 ))
  if [ $_hours -lt 24 ]; then
    echo "$_hours hours"
    exit 0
  fi
  _days=$(( _hours / 24 ))
  if [ $_days -lt 7 ]; then
    echo "$_days days"
    exit 0
  fi
  _weeks=$(( _days / 7 ))
  if [ $_weeks -lt 4 ]; then
    echo "$_weeks weeks"
    exit 0
  fi
  _months=$(( _days / 30 ))
  if [ $_months -lt 12 ]; then
    echo "$_months months"
    exit 0
  fi
  _years=$(( _days / 365 ))
  echo "$_years years"
  exit 0
}


# this feels hacky
all=$(mktemp)
grep -rl '^updated: ' | while read -r f
do
  # get the actual git modified date
  modified_pretty=$(git log -1 --date=iso-strict --format='%ad' -- "$f")

  # seconds since epoch
  modified_since_epoch=$(date --date="$modified_pretty" +%s)

  # get the date the file says
  updated=$(grep '^updated: ' "${f}" | awk '{print ""$2" "$3""}')
  seconds_since_updated_epoch=$(date --date="$updated" +%s)


  seconds_diff=$(( modified_since_epoch - seconds_since_updated_epoch ))

  printf "%s %s %s %s\\n" "$seconds_diff" "$f" "$updated" "$modified_pretty" >> "$all"
done

column -t "$all" | sort -g
echo "$all"

if [ "$return_code" -ne 0 ]; then
  echo "Possible errors. Check before commiting."
  exit 1
fi
exit 0
