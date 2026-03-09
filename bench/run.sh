#!/usr/bin/env bash
# Benchmark jx vs jq vs query-json using hyperfine
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA_DIR="$BENCH_DIR/data"
JX="${BENCH_DIR}/../_build/default/bin/main.exe"
JQ="jq"
QJ="query-json"

WARMUP=${WARMUP:-3}
MIN_RUNS=${MIN_RUNS:-10}

if [ ! -d "$DATA_DIR" ]; then
    bash "$BENCH_DIR/generate_data.sh"
fi

if ! command -v hyperfine &> /dev/null; then
    echo "Error: hyperfine is not installed"
    echo "Install with: pacman -S hyperfine, cargo install hyperfine, or brew install hyperfine"
    exit 1
fi

echo ""
echo "==================================="
echo "  jx vs jq vs query-json benchmark"
echo "==================================="
echo ""
echo "jx version:         $($JX --version 2>/dev/null || echo '0.1.0')"
echo "jq version:         $(jq --version)"
echo "query-json version: $(query-json --version 2>&1)"
echo "hyperfine:          $(hyperfine --version)"
echo "date:               $(date -I)"
echo ""

run() {
    local desc="$1"
    local data="$2"
    local jx_query="$3"
    local jq_query="$4"
    local qj_query="${5:-$jq_query}"

    echo "### $desc"
    echo ""

    local jx_cmd="$JX '$jx_query' '$data'"
    local jq_cmd="$JQ '$jq_query' '$data'"
    local qj_cmd="$QJ '$qj_query' '$data'"

    # verify all three produce output
    local qj_ok=true
    eval "$qj_cmd" > /dev/null 2>&1 || qj_ok=false

    if $qj_ok; then
        hyperfine --warmup "$WARMUP" --min-runs "$MIN_RUNS" -N \
            --command-name "jx" "$jx_cmd" \
            --command-name "jq" "$jq_cmd" \
            --command-name "qj" "$qj_cmd"
    else
        echo "(query-json: unsupported query, skipping)"
        hyperfine --warmup "$WARMUP" --min-runs "$MIN_RUNS" -N \
            --command-name "jx" "$jx_cmd" \
            --command-name "jq" "$jq_cmd"
    fi
    echo ""
}

# === SMALL OBJECT ===
echo "==================================="
echo "Small object (single record)"
echo "==================================="
echo ""

run "identity (.)" \
    "$DATA_DIR/small.json" "." "."

run "field access (.name)" \
    "$DATA_DIR/small.json" ".name" ".name"

run "nested field" \
    "$DATA_DIR/nested.json" ".user.address.city" ".user.address.city"

run "arithmetic (.age + .score)" \
    "$DATA_DIR/small.json" ".age + .score" ".age + .score"

run "conditional (if/then/else)" \
    "$DATA_DIR/small.json" \
    'if .age > 25 then "old" else "young"' \
    'if .age > 25 then "old" else "young" end' \
    'if .age > 25 then "old" else "young" end'

run "object construction {name, age}" \
    "$DATA_DIR/small.json" "{name, age}" "{name, age}"

# === MEDIUM (1K) ===
echo "==================================="
echo "Medium array (1,000 items)"
echo "==================================="
echo ""

run "identity (.)" \
    "$DATA_DIR/medium.json" "." "."

run "where (.price > 250)" \
    "$DATA_DIR/medium.json" \
    "where .price > 250" \
    '[.[] | select(.price > 250)]'

run "map (.name)" \
    "$DATA_DIR/medium.json" "map .name" '[.[] | .name]'

run "where | map pipeline" \
    "$DATA_DIR/medium.json" \
    "where .price > 250 | map .name" \
    '[.[] | select(.price > 250) | .name]'

run "sort_by (.price)" \
    "$DATA_DIR/medium.json" "sort_by .price" 'sort_by(.price)'

run "group_by (.city)" \
    "$DATA_DIR/medium.json" "group_by .city" 'group_by(.city)'

run "unique (map .city | unique)" \
    "$DATA_DIR/medium.json" "map .city | unique" '[.[] | .city] | unique'

run "count" \
    "$DATA_DIR/medium.json" "count" 'length'

run "sum (map .price | sum)" \
    "$DATA_DIR/medium.json" "map .price | sum" '[.[] | .price] | add'

run "take 10" \
    "$DATA_DIR/medium.json" "take 10" '.[:10]'

run "where | take 5 (early exit)" \
    "$DATA_DIR/medium.json" \
    "where .price > 400 | take 5" \
    '[limit(5; .[] | select(.price > 400))]' \
    '[.[] | select(.price > 400)] | .[:5]'

# === LARGE (10K) ===
echo "==================================="
echo "Large array (10,000 items)"
echo "==================================="
echo ""

run "identity (.)" \
    "$DATA_DIR/large.json" "." "."

run "where (.price > 250)" \
    "$DATA_DIR/large.json" \
    "where .price > 250" \
    '[.[] | select(.price > 250)]'

run "map (.name)" \
    "$DATA_DIR/large.json" "map .name" '[.[] | .name]'

run "where | map pipeline" \
    "$DATA_DIR/large.json" \
    "where .price > 250 | map .name" \
    '[.[] | select(.price > 250) | .name]'

run "sort_by (.price)" \
    "$DATA_DIR/large.json" "sort_by .price" 'sort_by(.price)'

run "sum (map .price | sum)" \
    "$DATA_DIR/large.json" "map .price | sum" '[.[] | .price] | add'

run "where | take 5 (early exit)" \
    "$DATA_DIR/large.json" \
    "where .price > 400 | take 5" \
    '[limit(5; .[] | select(.price > 400))]' \
    '[.[] | select(.price > 400)] | .[:5]'

# === XLARGE (100K) ===
echo "==================================="
echo "Extra-large array (100,000 items)"
echo "==================================="
echo ""

run "identity (.)" \
    "$DATA_DIR/xlarge.json" "." "."

run "where (.price > 250)" \
    "$DATA_DIR/xlarge.json" \
    "where .price > 250" \
    '[.[] | select(.price > 250)]'

run "map (.name)" \
    "$DATA_DIR/xlarge.json" "map .name" '[.[] | .name]'

run "sort_by (.price)" \
    "$DATA_DIR/xlarge.json" "sort_by .price" 'sort_by(.price)'

run "sum (map .price | sum)" \
    "$DATA_DIR/xlarge.json" "map .price | sum" '[.[] | .price] | add'

run "where | take 5 (early exit)" \
    "$DATA_DIR/xlarge.json" \
    "where .price > 400 | take 5" \
    '[limit(5; .[] | select(.price > 400))]' \
    '[.[] | select(.price > 400)] | .[:5]'

run "count" \
    "$DATA_DIR/xlarge.json" "count" 'length'

echo "==================================="
echo "Benchmark complete."
echo "==================================="
