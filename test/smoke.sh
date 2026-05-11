#!/usr/bin/env bash
set -euo pipefail

JBQ=${1:?usage: smoke.sh PATH_TO_JBQ}
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

check() {
  local name="$1"
  local expected="$2"
  shift 2
  local actual
  actual="$("$@")"
  if [[ "$actual" != "$expected" ]]; then
    echo "smoke test failed: $name" >&2
    echo "expected: $expected" >&2
    echo "actual:   $actual" >&2
    exit 1
  fi
}

cat > "$TMPDIR/data.json" <<'JSON'
{
  "name": "Alice",
  "age": 30,
  "user": {"address": {"city": "Kyiv"}},
  "items": [
    {"name": "A", "price": 50},
    {"name": "B", "price": 200}
  ],
  "orders": [
    {"items": [{"name": "W"}, {"name": "G"}]},
    {"items": [{"name": "D"}]}
  ]
}
JSON

check "identity from file" \
  '{"name":"Alice","age":30,"user":{"address":{"city":"Kyiv"}},"items":[{"name":"A","price":50},{"name":"B","price":200}],"orders":[{"items":[{"name":"W"},{"name":"G"}]},{"items":[{"name":"D"}]}]}' \
  "$JBQ" -c '.' "$TMPDIR/data.json"

actual="$(printf '%s\n' '{"ok":true}' | "$JBQ" -c '.')"
if [[ "$actual" != '{"ok":true}' ]]; then
  echo "smoke test failed: identity from stdin" >&2
  echo "expected: {\"ok\":true}" >&2
  echo "actual:   $actual" >&2
  exit 1
fi

actual="$(printf '%s\n' '{"name":"Alice"}' | "$JBQ" -r '.name')"
if [[ "$actual" != 'Alice' ]]; then
  echo "smoke test failed: field access from stdin" >&2
  echo "expected: Alice" >&2
  echo "actual:   $actual" >&2
  exit 1
fi

check "filter and map" \
  '["B"]' \
  "$JBQ" -c '.items | where .price > 100 | map .name' "$TMPDIR/data.json"

check "object construction" \
  '[{"name":"A","price":50},{"name":"B","price":200}]' \
  "$JBQ" -c '.items | map {name, price}' "$TMPDIR/data.json"

check "get exact path" \
  '"Kyiv"' \
  "$JBQ" -c 'get .user.address.city' "$TMPDIR/data.json"

check "has true" \
  'true' \
  "$JBQ" -c 'has .user.address.city' "$TMPDIR/data.json"

check "has false" \
  'false' \
  "$JBQ" -c 'has .user.address.zip' "$TMPDIR/data.json"

check "optional field access" \
  'null' \
  "$JBQ" -c '.missing?' "$TMPDIR/data.json"

check "pluck traversal" \
  '["W","G","D"]' \
  "$JBQ" -c 'pluck .orders .items .name' "$TMPDIR/data.json"

schema="$($JBQ --schema -c -f "$TMPDIR/data.json")"
if [[ "$schema" != *'"$schema":"https://json-schema.org/draft/2020-12/schema"'* ]]; then
  echo "smoke test failed: schema includes draft id" >&2
  echo "$schema" >&2
  exit 1
fi
if [[ "$schema" != *'"properties"'* || "$schema" != *'"items"'* ]]; then
  echo "smoke test failed: schema includes expected object/array structure" >&2
  echo "$schema" >&2
  exit 1
fi

sampled_schema="$($JBQ --schema -c -n 1 '.items' "$TMPDIR/data.json")"
if [[ "$sampled_schema" != *'"const":"A"'* || "$sampled_schema" == *'"const":"B"'* ]]; then
  echo "smoke test failed: schema sampling uses first element only" >&2
  echo "$sampled_schema" >&2
  exit 1
fi
