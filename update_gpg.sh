#!/usr/bin/env bash
set -euo pipefail

key_file="$(mktemp -d)/key.gpg"
# get key from keyserver

curl -so "$key_file" "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x3c799d26057b64e6d907b0acdb0e3c33491f91c9"

# update static file
cp "$key_file" ./provider/static/pgp.txt

# update pgp page
pgp_page=./provider/pages/pgp.markdown
sed -i '/    -----BEGIN PGP PUBLIC KEY BLOCK-----/,/    -----END PGP PUBLIC KEY BLOCK-----/d' $pgp_page
sed -i "s/updated: .*/updated: $(date +'%FT%TZ')/" $pgp_page

while read -r f; do
  # don't add unnessacary whitespace
  if [ "$f" == "" ]; then
    echo "" >> ./provider/pages/pgp.markdown
  else
    echo "    $f" >> ./provider/pages/pgp.markdown
  fi
done <"$key_file"
