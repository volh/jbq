#!/usr/bin/env bash
# Benchmark jx vs jq vs query-json — time and memory
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA_DIR="$BENCH_DIR/data"
JX="${BENCH_DIR}/../_build/default/bin/main.exe"
JQ="jq"
QJ="query-json"
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

printf "\n${BOLD}jx vs jq vs query-json benchmark${NC}\n"
printf "${DIM}%d runs per test, %d warmup runs discarded${NC}\n" "$RUNS" "$WARMUP"
printf "${DIM}jq version: $(jq --version)${NC}\n"
printf "${DIM}query-json version: $(query-json --version 2>&1)${NC}\n\n"

declare -a RESULT_NAMES=()
declare -a RESULT_JX_TIME=()
declare -a RESULT_JQ_TIME=()
declare -a RESULT_QJ_TIME=()
declare -a RESULT_JX_MEM=()
declare -a RESULT_JQ_MEM=()
declare -a RESULT_QJ_MEM=()

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
    local qj_query="${5:-$jq_query}"
    local jx_cmd="$JX '$jx_query' '$data'"
    local jq_cmd="$JQ '$jq_query' '$data'"
    local qj_cmd="$QJ '$qj_query' '$data'"

    printf "${CYAN}%-35s${NC} " "$name"

    # Verify jx and jq
    local jx_out jq_out qj_out
    jx_out=$(eval "$jx_cmd" 2>/dev/null) || { printf "${RED}jx error${NC}\n"; return; }
    jq_out=$(eval "$jq_cmd" 2>/dev/null) || { printf "${RED}jq error${NC}\n"; return; }
    qj_out=$(eval "$qj_cmd" 2>/dev/null) || qj_out="ERROR"

    # Time
    local jx_us jq_us qj_us
    jx_us=$(run_timed "$jx_cmd")
    jq_us=$(run_timed "$jq_cmd")
    if [ "$qj_out" != "ERROR" ]; then
        qj_us=$(run_timed "$qj_cmd")
    else
        qj_us=0
    fi

    # Memory
    local jx_mem jq_mem qj_mem
    jx_mem=$(run_memory "$jx_cmd")
    jq_mem=$(run_memory "$jq_cmd")
    if [ "$qj_out" != "ERROR" ]; then
        qj_mem=$(run_memory "$qj_cmd")
    else
        qj_mem=0
    fi

    local jx_t_fmt=$(fmt_time "$jx_us")
    local jq_t_fmt=$(fmt_time "$jq_us")
    local jx_m_fmt=$(fmt_mem "$jx_mem")
    local jq_m_fmt=$(fmt_mem "$jq_mem")

    local qj_t_fmt qj_m_fmt
    if [ "$qj_us" -gt 0 ]; then
        qj_t_fmt=$(fmt_time "$qj_us")
        qj_m_fmt=$(fmt_mem "$qj_mem")
    else
        qj_t_fmt="n/a"
        qj_m_fmt="n/a"
    fi

    local jx_jq_ratio=$(awk -v a="$jx_us" -v b="$jq_us" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "n/a" }')
    local jx_qj_ratio
    if [ "$qj_us" -gt 0 ]; then
        jx_qj_ratio=$(awk -v a="$jx_us" -v b="$qj_us" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "n/a" }')
    else
        jx_qj_ratio="n/a"
    fi

    local tc=$(ratio_color "$jx_jq_ratio")
    local TC
    case "$tc" in green) TC="$GREEN";; red) TC="$RED";; *) TC="$YELLOW";; esac

    local tc2 TC2
    if [ "$jx_qj_ratio" != "n/a" ]; then
        tc2=$(ratio_color "$jx_qj_ratio")
        case "$tc2" in green) TC2="$GREEN";; red) TC2="$RED";; *) TC2="$YELLOW";; esac
    else
        TC2="$DIM"
    fi

    printf "jx:${BOLD}%8s${NC}  jq:${BOLD}%8s${NC}  qj:${BOLD}%8s${NC}  ${TC}jx/jq:%5s${NC}  ${TC2}jx/qj:%5s${NC}  | jx:${BOLD}%6s${NC}  jq:${BOLD}%6s${NC}  qj:${BOLD}%6s${NC}\n" \
        "$jx_t_fmt" "$jq_t_fmt" "$qj_t_fmt" "$jx_jq_ratio" "$jx_qj_ratio" "$jx_m_fmt" "$jq_m_fmt" "$qj_m_fmt"

    RESULT_NAMES+=("$name")
    RESULT_JX_TIME+=("$jx_us")
    RESULT_JQ_TIME+=("$jq_us")
    RESULT_QJ_TIME+=("$qj_us")
    RESULT_JX_MEM+=("$jx_mem")
    RESULT_JQ_MEM+=("$jq_mem")
    RESULT_QJ_MEM+=("$qj_mem")
}

printf "${BOLD}%-35s %-60s  %-30s${NC}\n" "" "TIME (median)" "MEMORY (peak RSS)"
printf "%s\n" "$(printf '%.0s-' {1..140})"

# === SMALL OBJECT ===
printf "\n${BOLD}Small object (single record)${NC}\n"
bench "identity (.)" \
    "$DATA_DIR/small.json" "." "."
bench "field access (.name)" \
    "$DATA_DIR/small.json" ".name" ".name"
bench "nested field" \
    "$DATA_DIR/nested.json" ".user.address.city" ".user.address.city"
bench "arithmetic (.age + .score)" \
    "$DATA_DIR/small.json" ".age + .score" ".age + .score"
bench "conditional (if/then/else)" \
    "$DATA_DIR/small.json" \
    'if .age > 25 then "old" else "young"' \
    'if .age > 25 then "old" else "young" end' \
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
    '[limit(5; .[] | select(.price > 400))]' \
    '[.[] | select(.price > 400)] | .[:5]'
bench "map | take 5 (early exit)" \
    "$DATA_DIR/medium.json" \
    "map .name | take 5" \
    '[limit(5; .[] | .name)]' \
    '[.[] | .name] | .[:5]'

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
    '[limit(5; .[] | select(.price > 400))]' \
    '[.[] | select(.price > 400)] | .[:5]'
bench "map | take 5 (early exit)" \
    "$DATA_DIR/large.json" \
    "map .name | take 5" \
    '[limit(5; .[] | .name)]' \
    '[.[] | .name] | .[:5]'
bench "where | map | take 3 (early)" \
    "$DATA_DIR/large.json" \
    "where .price > 400 | map .name | take 3" \
    '[limit(3; .[] | select(.price > 400) | .name)]' \
    '[.[] | select(.price > 400) | .name] | .[:3]'

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
    '[limit(5; .[] | select(.price > 400))]' \
    '[.[] | select(.price > 400)] | .[:5]'
bench "where | map | take 3 (early)" \
    "$DATA_DIR/xlarge.json" \
    "where .price > 400 | map .name | take 3" \
    '[limit(3; .[] | select(.price > 400) | .name)]' \
    '[.[] | select(.price > 400) | .name] | .[:3]'

# === Summary ===
printf "\n%s\n" "$(printf '%.0s=' {1..140})"
printf "${BOLD}Summary${NC}\n\n"

jx_vs_jq_faster=0
jq_faster=0
jx_jq_tied=0
jx_vs_qj_faster=0
qj_faster=0
jx_qj_tied=0

for ((i=0; i<${#RESULT_NAMES[@]}; i++)); do
    tr=$(awk -v a="${RESULT_JX_TIME[$i]}" -v b="${RESULT_JQ_TIME[$i]}" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "1" }')
    tc=$(ratio_color "$tr")
    case "$tc" in green) jx_vs_jq_faster=$((jx_vs_jq_faster+1));; red) jq_faster=$((jq_faster+1));; *) jx_jq_tied=$((jx_jq_tied+1));; esac

    if [ "${RESULT_QJ_TIME[$i]}" -gt 0 ]; then
        qr=$(awk -v a="${RESULT_JX_TIME[$i]}" -v b="${RESULT_QJ_TIME[$i]}" 'BEGIN { if(b>0) printf "%.2f", a/b; else print "1" }')
        qc=$(ratio_color "$qr")
        case "$qc" in green) jx_vs_qj_faster=$((jx_vs_qj_faster+1));; red) qj_faster=$((qj_faster+1));; *) jx_qj_tied=$((jx_qj_tied+1));; esac
    fi
done

total=${#RESULT_NAMES[@]}
printf "  vs jq:         ${GREEN}jx faster: %d${NC}  ${RED}jq faster: %d${NC}  ${YELLOW}tied: %d${NC}  (of %d tests)\n" \
    "$jx_vs_jq_faster" "$jq_faster" "$jx_jq_tied" "$total"
printf "  vs query-json: ${GREEN}jx faster: %d${NC}  ${RED}qj faster: %d${NC}  ${YELLOW}tied: %d${NC}  (of %d tests)\n" \
    "$jx_vs_qj_faster" "$qj_faster" "$jx_qj_tied" "$total"

printf "\n${DIM}ratio = jx/other (green < 0.9 = jx wins, red > 1.1 = other wins)${NC}\n"
