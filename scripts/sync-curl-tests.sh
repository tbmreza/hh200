#!/bin/sh
set -eu

tag="${CURL_TEST_TAG:-curl-8_23_0}"
base="https://raw.githubusercontent.com/curl/curl/${tag}"
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
data_dir="$root/test/curl/data"

mkdir -p "$data_dir"
for number in 207 214; do
  curl --fail --location --silent --show-error \
    "$base/tests/data/test${number}" > "$data_dir/test${number}"
done
curl --fail --location --silent --show-error "$base/COPYING" > "$root/test/curl/COPYING"
printf '%s\n' "$tag" > "$root/test/curl/UPSTREAM-TAG"

