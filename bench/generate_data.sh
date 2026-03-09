#!/usr/bin/env bash
# Generate test JSON data for benchmarks
set -euo pipefail

BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA_DIR="$BENCH_DIR/data"
mkdir -p "$DATA_DIR"

# Small object (single record)
cat > "$DATA_DIR/small.json" <<'EOF'
{"name": "Alice", "age": 30, "email": "alice@example.com", "city": "NYC", "score": 95.5}
EOF

# Nested object
cat > "$DATA_DIR/nested.json" <<'EOF'
{"user": {"name": "Alice", "address": {"city": "NYC", "zip": "10001"}, "prefs": {"theme": "dark"}}}
EOF

# Medium array (1000 items)
python3 -c "
import json, random
data = []
names = ['Alice','Bob','Carol','Dave','Eve','Frank','Grace','Heidi','Ivan','Judy']
cities = ['NYC','LA','Chicago','Houston','Phoenix','Philly','San Antonio','San Diego','Dallas','San Jose']
for i in range(1000):
    data.append({
        'id': i,
        'name': names[i % len(names)],
        'age': 20 + (i % 50),
        'city': cities[i % len(cities)],
        'price': round(random.uniform(10, 500), 2),
        'score': round(random.uniform(0, 100), 2),
        'active': i % 3 != 0
    })
print(json.dumps(data))
" > "$DATA_DIR/medium.json"

# Large array (10000 items)
python3 -c "
import json, random
data = []
names = ['Alice','Bob','Carol','Dave','Eve','Frank','Grace','Heidi','Ivan','Judy']
cities = ['NYC','LA','Chicago','Houston','Phoenix','Philly','San Antonio','San Diego','Dallas','San Jose']
for i in range(10000):
    data.append({
        'id': i,
        'name': names[i % len(names)],
        'age': 20 + (i % 50),
        'city': cities[i % len(cities)],
        'price': round(random.uniform(10, 500), 2),
        'score': round(random.uniform(0, 100), 2),
        'active': i % 3 != 0
    })
print(json.dumps(data))
" > "$DATA_DIR/large.json"

# Very large array (100000 items) for stress testing
python3 -c "
import json, random
data = []
names = ['Alice','Bob','Carol','Dave','Eve','Frank','Grace','Heidi','Ivan','Judy']
cities = ['NYC','LA','Chicago','Houston','Phoenix','Philly','San Antonio','San Diego','Dallas','San Jose']
for i in range(100000):
    data.append({
        'id': i,
        'name': names[i % len(names)],
        'age': 20 + (i % 50),
        'city': cities[i % len(cities)],
        'price': round(random.uniform(10, 500), 2),
        'score': round(random.uniform(0, 100), 2),
        'active': i % 3 != 0
    })
print(json.dumps(data))
" > "$DATA_DIR/xlarge.json"

echo "Generated test data:"
for f in "$DATA_DIR"/*.json; do
    echo "  $(basename "$f"): $(wc -c < "$f") bytes"
done
