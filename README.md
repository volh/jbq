# jbq

JSON Bourne Query — a jq replacement with a genuinely better query language.

Not "jq in Rust" or "jq in Go" — a new language that's readable, concise, and unambiguous. Every existing alternative (jaq, gojq, fq, query-json) kept jq's syntax and competed on runtime. jbq competes on **language design**.

```bash
# jq
.items[] | select(.price > 100) | {name, display: (.name | ascii_downcase | .[0:20])}

# jbq
items | where .price > 100 | map {name, display: .name | lower | truncate 20}
```

## Install

Requires:
- OCaml 5.4+
- opam
- a system simdjson installation (headers + shared library)

```bash
opam install . --deps-only --with-test -y
make build
```

The binary lands at `_build/default/bin/main.exe`. Copy or symlink it as `jbq`.

## Usage

```bash
# Read from file
jbq 'items | where .price > 100' data.json

# Read from stdin
curl -s api.example.com/data | jbq '.users | map {name, email}'

# Identity (pretty-print JSON)
jbq '.' data.json

# Raw string output (no quotes)
jbq -r '.name' data.json

# Compact output
jbq -c '.' data.json

# Infer schema for a query result
jbq --schema '.users' data.json

# Infer schema for a file with the default identity query
jbq --schema -f data.json

# Sample only the first 100 elements during schema inference
jbq --schema -n 100 '.items' data.json

# Drop const/enum annotations and keep only base types
jbq --schema --no-const --no-enum '.items' data.json
```

## Schema Inference

`jbq --schema` evaluates the query first, then infers a JSON Schema
(draft 2020-12) from the result.

```bash
# Schema for the whole input
jbq --schema '.' data.json

# Schema for a nested collection
jbq --schema '.users' data.json

# File-only mode: use the default identity query
jbq --schema -f data.json

# Limit inference to the first N elements
jbq --schema -n 100 '.items' data.json

# Suppress const and enum annotations
jbq --schema --no-const --no-enum '.items' data.json
```

CLI rule of thumb:
- first positional argument is always `QUERY`
- second positional argument is always `FILE`
- if you want schema from a file with no explicit query, use `-f` / `--file`

## Three Rules

1. `.field` — dot is always data (field access)
2. `bare_word` — always a function
3. `|` — always composition (pipe)

No exceptions.

## Language

### Field access

```
.name                    # top-level field
.user.address.city       # nested
.[0]                     # array index
.[1:5]                   # slice
.name?                   # optional (null if missing, no error)
```

Postfix access is whitespace-sensitive: `.items[0].name` is one chained
expression, while `.items [0]` is two expressions and does not parse as
indexing.

### Pipes

```
.items | where .price > 100 | map .name
.name | lower | truncate 20
```

### Exact Access and Traversal

Paths stay exact. When cardinality changes, the query must say so with a
function.

```
get .user.address.city
has .config.features.dark_mode
pluck .orders .items .name
```

### Function arguments

```
pick .name .email        # whitespace separates arguments
pick .name; .email       # semicolons work as explicit separators too
```

Whitespace ends an implicit argument. Postfix operators (`.`, `[]`, `?`) only
bind when attached to the expression they extend.

### Object construction (with punning)

```
{name, price}                    # => {name: .name, price: .price}
{name, total: .price * .qty}     # mix punned + explicit
```

### Operators

```
.price + .tax            # arithmetic: + - * / %
.price > 100             # comparison: == != < > <= >=
.active && .verified     # logical: && || !
.first ++ " " ++ .last  # string concat (not +)
.email ?? "none"         # null coalescing
```

Large integers are preserved exactly. Integer-only arithmetic stays exact even
past native `int` range.

### Conditionals and let bindings

```
if .price > 100 then "expensive" else "cheap"
let total = .price * .qty in {name, total}
```

### Lambdas

```
items | map (x => {name: x.name, total: x.price * x.qty})
```

### String interpolation

```
`Hello, ${.name}!`
```

## Built-in Functions

### Collection

| Function | Description |
|---|---|
| `where expr` | filter by predicate |
| `map expr` | transform each element |
| `flatmap expr` | transform each element, flatten one level |
| `sort_by expr` | sort by expression |
| `group_by expr` | group into object |
| `unique` | deduplicate, preserve order |
| `flatten` | flatten nested arrays |
| `reverse` | reverse order |

### Slicing

| Function | Description |
|---|---|
| `first` | first element |
| `last` | last element |
| `take n` | first n |
| `skip n` | skip n |
| `count` | length |

### Aggregation

| Function | Description |
|---|---|
| `sum` | sum numbers |
| `min` | minimum |
| `max` | maximum |
| `avg` | average |

### String

| Function | Description |
|---|---|
| `lower` | lowercase |
| `upper` | uppercase |
| `trim` | strip whitespace |
| `truncate n` | safe truncate |
| `split s` | split string |
| `join s` | join array |

### Object

| Function | Description |
|---|---|
| `get path` | exact lookup |
| `has path` | boolean exact-path existence check |
| `pluck p1 p2 ...` | explicit traversal across nested collections |
| `keys` | extract keys |
| `values` | extract values |
| `pick k1 k2` | select specific keys |
| `omit k1 k2` | remove specific keys |

### Generators

| Function | Description |
|---|---|
| `range` | infinite lazy sequence 0, 1, 2, ... |
| `range n` | [0, n) |
| `range m n` | [m, n) |

## jbq vs jq

|                   | jq                                      | jbq                                                            |
|-------------------|-----------------------------------------|----------------------------------------------------------------|
| **Iteration**     | Explicit: `.items[]`                    | Implicit: `items` (collection functions iterate automatically) |
| **Filter**        | `select(.price > 100)`                  | `where .price > 100`                                          |
| **Transform**     | `map(.name)` or `.[] \| .name`          | `map .name`                                                    |
| **Object build**  | `{name: .name, price: .price}`          | `{name, price}` (punning)                                      |
| **String concat** | `(.first) + " " + (.last)`              | `.first ++ " " ++ .last`                                       |
| **String ops**    | `ascii_downcase`, `ltrimstr`            | `lower`, `trim`, `truncate`                                    |
| **Null access**   | Silent null propagation                 | Error by default, `?` for optional                             |
| **Exact/traverse**| `.users[0].name` / `.users[].name`      | `get .users[0].name` / `pluck .users .name`                   |
| **Sort**          | `sort_by(.price)`                       | `sort_by .price`                                               |
| **Subsets**       | `{name, email}` (only top-level)        | `pick .name .email` / `omit .password`                         |
| **Arg separator** | Nested parens                           | Whitespace or semicolons: `pick .name .email`                  |
| **Types**         | Implicit coercion                       | Strict: `++` for strings, `+` for numbers                      |
| **Evaluation**    | Generator-based (implicit backtracking) | Single-value with lazy sequences                               |

### What jq gets right

jq is battle-tested, ubiquitous, and its streaming model handles arbitrarily large input. The `@base64`, `@uri`, `@csv` format strings are genuinely useful. `--slurp` and `--jsonargs` cover real CLI needs. jbq doesn't aim to replace all of that on day one.

Current parser status:
- `jbq` now uses a native simdjson-based input path by default
- top-level array queries that fit the supported pipeline subset use a streamed
  `Value.Seq` path
- other inputs fall back to a full native `Value.t` parse

### Where jbq diverges

jq's generator semantics are powerful but produce puzzling behavior. `null | .x` silently returns `null`. `empty` propagates invisibly. The difference between `.[]` and `map(.)` is subtle. `if-then` without `else` is a filter, not a conditional.

jbq makes one bet: a readable language with predictable semantics is worth more than compatibility with jq's 10-year ecosystem. If you know what `where`, `map`, and `|` mean, you can read any jbq query cold.

## Transducer Pipelines

Chained collection operations (`where`, `map`, `take`, `skip`, `unique`, `flatten`) don't create intermediate collections. They fuse into a single transducer pipeline that processes elements one at a time.

```bash
# This is a single pass, not three:
items | where .price > 100 | map .name | take 5
```

A transducer is a step function `(accumulator, item) -> (accumulator, signal)` where the signal is either `continue` or `done`. Composition chains these steps — `filter` wraps `map` wraps `take` — so each element flows through the full pipeline before the next one enters.

`take n` returns `done` after n elements, which halts the entire pipeline immediately. No remaining elements are evaluated. This means `range | where (. % 2 == 0) | take 5` terminates despite the infinite source — `take` signals `done` after 5 results, and the pipeline stops pulling from `range`.

Aggregation functions (`sum`, `avg`, `count`) also fuse — they fold directly over the transducer without materializing a list. Functions that inherently need all elements (`sort_by`, `group_by`, `reverse`, `last`) realize the sequence first, then operate eagerly.

```
# Fused (single pass, no intermediate allocation):
where pred | map f | take n | sum

# Realized then eager (must see all elements):
where pred | sort_by .price
```

The user never sees this. Input is arrays, output is arrays. Transducers are an internal optimization.

## Development

```bash
make build        # build
make test         # run tests
make dev          # watch mode
make fmt          # format code
make clean        # clean build artifacts
```

## Architecture

```
Input -> simdjson native parser -> Value.t / Value.Seq -> Lexer (sedlex) -> Parser (Pratt) -> Interpreter -> JSON output
```

Written in OCaml. MIT license.
