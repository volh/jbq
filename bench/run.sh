#!/usr/bin/env bash
# Benchmark jx vs jq — time and memory
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA_DIR="$BENCH_DIR/data"
JX="${BENCH_DIR}/../_build/default/bin/main.exe"
JQ="jq"
RUNS=20
WARMUP=3

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

if [ ! -d "$DATA_DIR" ]; then
    bash "$BENCH_DIR/generate_data.sh"
fi

printf "\n${BOLD}jx vs jq benchmark${NC}\n"
printf "${DIM}%d runs per test, %d warmup runs discarded${NC}\n" "$RUNS" "$WARMUP"
printf "${DIM}jq version: $(jq --version)${NC}\n\n"

declare -a RESULT_NAMES=()
declare -a RESULT_JX_TIME=()
declare -a RESULT_JQ_TIME=()
declare -a RESULT_JX_MEM=()
declare -a RESULT_JQ_MEM=()

fmt_time() {
    local us="$1"
    awk -v us="$us" 'BEGIN {
        if (us >= 1000000) printf "%.1fs", us/1000000
        else if (us >= 1000) printf "%.1fms", us/1000
        else printf "%dus", us
    }'
}

fmt_mem() {
    local kb="$1"
    awk -v kb="$kb" 'BEGIN {
        if (kb >= 1024) printf "%.1fM", kb/1024
        else printf "%dK", kb
    }'
}

ratio_color() {
    local ratio="$1"
    awk -v r="$ratio" 'BEGIN {
        if (r < 0.9) print "green"
        else if (r > 1.1) print "red"
        else print "yellow"
    }'
}

run_timed() {
    local cmd="$1"
    local times=()

    for ((i=0; i<WARMUP; i++)); do
        eval "$cmd" > /dev/null 2>&1 || true
    done

    for ((i=0; i<RUNS; i++)); do
        local start end elapsed
        start=$(date +%s%N)
        eval "$cmd" > /dev/null 2>&1
        end=$(date +%s%N)
        elapsed=$(( (end - start) / 1000 ))
        times+=("$elapsed")
    done

    IFS=$'\n' sorted=($(sort -n <<<"${times[*]}")); unset IFS
    echo "${sorted[$((RUNS / 2))]}"
}

run_memory() {
    local cmd="$1"
    /usr/bin/time -v bash -c "$cmd > /dev/null 2>&1" 2>&1 | grep "Maximum resident" | grep -oP '\d+'
}

bench() {
    local name="$1"
    local data="$2"
    local jx_query="$3"
    local jq_query="$4"
    local jx_cmd="$JX '$jx_query' '$data'"
    local jq_cmd="$JQ '$jq_query' '$data'"

    printf "${CYAN}%-42s${NC} " "$name"

    # Verify
    local jx_out jq_out
    jx_out=$(eval "$jx_cmd" 2>/dev/null) || { printf "${RED}jx error${NC}\n"; return; }
    jq_out=$(eval "$jq_cmd" 2>/dev/null) || { printf "${RED}jq error${NC}\n"; return; }

    # Time
    local jx_us jq_us
    jx_us=$(run_timed "$jx_cmd")
    jq_us=$(run_timed "$jq_cmd")

    # Memory
    local jx_mem jq_mem
    jx_mem=$(run_memory "$jx_cmd")
    jq_mem=$(run_memory "$jq_cmd")

    local jx_t_fmt=$(fmt_time "$jx_us")
    local jq_t_fmt=$(fmt_time "$jq_us")
    local jx_m_fmt=$(fmt_mem "$jx_mem")
    local jq_m_fmt=$(fmt_mem "$jq_mem")

    local time_ratio=$(awk -v a="$jx_us" -v b="$jq_us" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "n/a" }')
    local mem_ratio=$(awk -v a="$jx_mem" -v b="$jq_mem" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "n/a" }')

    local tc=$(ratio_color "$time_ratio")
    local mc=$(ratio_color "$mem_ratio")
    local TC MC
    case "$tc" in green) TC="$GREEN";; red) TC="$RED";; *) TC="$YELLOW";; esac
    case "$mc" in green) MC="$GREEN";; red) MC="$RED";; *) MC="$YELLOW";; esac

    printf "jx:${BOLD}%8s${NC}  jq:${BOLD}%8s${NC}  ${TC}%5sx${NC}  | jx:${BOLD}%6s${NC}  jq:${BOLD}%6s${NC}  ${MC}%5sx${NC}\n" \
        "$jx_t_fmt" "$jq_t_fmt" "$time_ratio" "$jx_m_fmt" "$jq_m_fmt" "$mem_ratio"

    RESULT_NAMES+=("$name")
    RESULT_JX_TIME+=("$jx_us")
    RESULT_JQ_TIME+=("$jq_us")
    RESULT_JX_MEM+=("$jx_mem")
    RESULT_JQ_MEM+=("$jq_mem")
}

printf "${BOLD}%-42s %-35s   %-30s${NC}\n" "" "TIME (median)" "MEMORY (peak RSS)"
printf "%s\n" "$(printf '%.0s-' {1..115})"

# === SMALL OBJECT ===
printf "\n${BOLD}Small object (single record)${NC}\n"
bench "identity (.)" \
    "$DATA_DIR/small.json" "." "."
bench "field access (.name)" \
    "$DATA_DIR/small.json" ".name" ".name"
bench "nested field (.user.address.city)" \
    "$DATA_DIR/nested.json" ".user.address.city" ".user.address.city"
bench "arithmetic (.age + .score)" \
    "$DATA_DIR/small.json" ".age + .score" ".age + .score"
bench "conditional (if/then/else)" \
    "$DATA_DIR/small.json" \
    'if .age > 25 then "old" else "young"' \
    'if .age > 25 then "old" else "young" end'
bench "object construction {name, age}" \
    "$DATA_DIR/small.json" "{name, age}" "{name, age}"

# === MEDIUM (1K) ===
printf "\n${BOLD}Medium array (1,000 items)${NC}\n"
bench "identity (.)" \
    "$DATA_DIR/medium.json" "." "."
bench "select/where (.price > 250)" \
    "$DATA_DIR/medium.json" \
    "where .price > 250" \
    '[.[] | select(.price > 250)]'
bench "map (.name)" \
    "$DATA_DIR/medium.json" "map .name" '[.[] | .name]'
bench "where | map pipeline" \
    "$DATA_DIR/medium.json" \
    "where .price > 250 | map .name" \
    '[.[] | select(.price > 250) | .name]'
bench "sort_by (.price)" \
    "$DATA_DIR/medium.json" "sort_by .price" 'sort_by(.price)'
bench "group_by (.city)" \
    "$DATA_DIR/medium.json" "group_by .city" 'group_by(.city)'
bench "unique (map .city | unique)" \
    "$DATA_DIR/medium.json" "map .city | unique" '[.[] | .city] | unique'
bench "count" \
    "$DATA_DIR/medium.json" "count" 'length'
bench "sum (map .price | sum)" \
    "$DATA_DIR/medium.json" "map .price | sum" '[.[] | .price] | add'
bench "take 10" \
    "$DATA_DIR/medium.json" "take 10" '.[:10]'
bench "skip 990" \
    "$DATA_DIR/medium.json" "skip 990" '.[990:]'
bench "where | take 5 (early exit)" \
    "$DATA_DIR/medium.json" \
    "where .price > 400 | take 5" \
    '[limit(5; .[] | select(.price > 400))]'
bench "map | take 5 (early exit)" \
    "$DATA_DIR/medium.json" \
    "map .name | take 5" \
    '[limit(5; .[] | .name)]'

# === LARGE (10K) ===
printf "\n${BOLD}Large array (10,000 items)${NC}\n"
bench "identity (.)" \
    "$DATA_DIR/large.json" "." "."
bench "select/where (.price > 250)" \
    "$DATA_DIR/large.json" \
    "where .price > 250" \
    '[.[] | select(.price > 250)]'
bench "map (.name)" \
    "$DATA_DIR/large.json" "map .name" '[.[] | .name]'
bench "where | map pipeline" \
    "$DATA_DIR/large.json" \
    "where .price > 250 | map .name" \
    '[.[] | select(.price > 250) | .name]'
bench "sort_by (.price)" \
    "$DATA_DIR/large.json" "sort_by .price" 'sort_by(.price)'
bench "sum (map .price | sum)" \
    "$DATA_DIR/large.json" "map .price | sum" '[.[] | .price] | add'
bench "where | take 5 (early exit)" \
    "$DATA_DIR/large.json" \
    "where .price > 400 | take 5" \
    '[limit(5; .[] | select(.price > 400))]'
bench "map | take 5 (early exit)" \
    "$DATA_DIR/large.json" \
    "map .name | take 5" \
    '[limit(5; .[] | .name)]'
bench "where | map | take 3 (early exit)" \
    "$DATA_DIR/large.json" \
    "where .price > 400 | map .name | take 3" \
    '[limit(3; .[] | select(.price > 400) | .name)]'

# === XLARGE (100K) ===
printf "\n${BOLD}Extra-large array (100,000 items)${NC}\n"
bench "identity (.)" \
    "$DATA_DIR/xlarge.json" "." "."
bench "select/where (.price > 250)" \
    "$DATA_DIR/xlarge.json" \
    "where .price > 250" \
    '[.[] | select(.price > 250)]'
bench "map (.name)" \
    "$DATA_DIR/xlarge.json" "map .name" '[.[] | .name]'
bench "sort_by (.price)" \
    "$DATA_DIR/xlarge.json" "sort_by .price" 'sort_by(.price)'
bench "sum (map .price | sum)" \
    "$DATA_DIR/xlarge.json" "map .price | sum" '[.[] | .price] | add'
bench "where | take 5 (early exit)" \
    "$DATA_DIR/xlarge.json" \
    "where .price > 400 | take 5" \
    '[limit(5; .[] | select(.price > 400))]'
bench "where | map | take 3 (early exit)" \
    "$DATA_DIR/xlarge.json" \
    "where .price > 400 | map .name | take 3" \
    '[limit(3; .[] | select(.price > 400) | .name)]'

# === Summary ===
printf "\n%s\n" "$(printf '%.0s=' {1..115})"
printf "${BOLD}Summary${NC}\n\n"

jx_faster=0
jq_faster=0
tied=0
jx_less_mem=0
jq_less_mem=0
mem_tied=0

for ((i=0; i<${#RESULT_NAMES[@]}; i++)); do
    tr=$(awk -v a="${RESULT_JX_TIME[$i]}" -v b="${RESULT_JQ_TIME[$i]}" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "1" }')
    tc=$(ratio_color "$tr")
    case "$tc" in green) ((jx_faster++));; red) ((jq_faster++));; *) ((tied++));; esac

    mr=$(awk -v a="${RESULT_JX_MEM[$i]}" -v b="${RESULT_JQ_MEM[$i]}" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "1" }')
    mc=$(ratio_color "$mr")
    case "$mc" in green) ((jx_less_mem++));; red) ((jq_less_mem++));; *) ((mem_tied++));; esac
done

total=${#RESULT_NAMES[@]}
printf "  Time:   ${GREEN}jx faster: %d${NC}  ${RED}jq faster: %d${NC}  ${YELLOW}tied: %d${NC}  (of %d tests)\n" \
    "$jx_faster" "$jq_faster" "$tied" "$total"
printf "  Memory: ${GREEN}jx leaner: %d${NC}  ${RED}jq leaner: %d${NC}  ${YELLOW}tied: %d${NC}  (of %d tests)\n" \
    "$jx_less_mem" "$jq_less_mem" "$mem_tied" "$total"

printf "\n${DIM}ratio = jx/jq (green < 0.9 = jx wins, red > 1.1 = jq wins)${NC}\n"
