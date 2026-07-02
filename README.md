# jbq

JSON Bourne Query (`jbq`) is a JSON query CLI with a compact, explicit query
language for filtering, reshaping, inspecting JSON, and inferring JSON Schema.

It came out of working with jq. jq is powerful and mature, but its language can
be hard to read once queries move beyond small filters. `jbq` keeps the same
practical domain while making collection operations and traversal more visible:
`where`, `map`, `flatmap`, `pluck`, exact paths by default, and separate numeric
and string operators.

`jbq` is not jq-compatible. It is a separate language for common JSON querying
tasks.

```bash
# jq
.items[] | select(.price > 100) | {name, display: (.name | ascii_downcase | .[0:20])}

# jbq
.items | where .price > 100 | map {name, display: .name | lower | truncate 20}
```

## Current state

The CLI works today from a source build. The current implementation:

- reads JSON from a file or stdin
- uses a native simdjson input path
- streams compatible top-level array pipelines
- preserves large integers exactly
- supports JSON Schema inference for query results
- supports lazy pipelines, including infinite `range` pipelines bounded by
  `take`, `first`, `sum`, or `count`

The project is still pre-1.0. The language and CLI should be treated as subject
to change until the first tagged release.

## Install

Requires:

- OCaml 5.4+
- opam
- a system simdjson installation (headers and shared library)

Install simdjson with your system package manager first:

```bash
# Arch/CachyOS
sudo pacman -S simdjson

# Debian/Ubuntu
sudo apt install libsimdjson-dev

# macOS
brew install simdjson
```

Then create a local opam switch with the project dependencies and build:

```bash
make init
make build
```

If you already have a suitable local switch, use:

```bash
make install-deps
make build
```

The binary lands at `_build/default/bin/main.exe`. Copy or symlink it as `jbq`.

## Usage

```bash
# Read from file
jbq '.items | where .price > 100' data.json

# Read from stdin
curl -s api.example.com/data | jbq '.users | map {name, email}'

# Identity query
jbq '.' data.json

# Raw string output
jbq -r '.name' data.json

# Compact JSON output
jbq -c '.' data.json

# Infer JSON Schema for a query result
jbq --schema '.users' data.json

# Infer JSON Schema for a whole file
jbq --schema -f data.json

# Infer from only the first 100 array elements
jbq --schema -n 100 '.items' data.json

# Keep schemas structural by dropping const/enum annotations
jbq --schema --no-const --no-enum '.items' data.json
```

CLI argument rules:

- first positional argument is `QUERY`
- second positional argument is `FILE`
- if you want schema inference from a file with no explicit query, use
  `-f` / `--file`

## Query model

`jbq` uses a few explicit rules:

- `.` is the current value.
- `.field`, `.[0]`, and `.[1:5]` are exact accessors.
- `|` passes the result of the left expression into the right expression.
- Collection operations are functions: `where`, `map`, `flatmap`, `take`, etc.
- Traversal across nested arrays is explicit with `pluck` or `flatmap`.
- Missing fields are errors by default; optional access uses `?`.
- `+` is numeric addition. `++` is string concatenation.

## Language examples

### Field access

```jq
.name
.user.address.city
.[0]
.[-1]
.[1:5]
.name?
```

Postfix access is whitespace-sensitive: `.items[0].name` is one chained
expression, while `.items [0]` is not indexing syntax.

### Pipes and collection functions

```jq
.items | where .price > 100
.items | where .price > 100 | map .name
.items | sort_by .price | take 10
range | where (. % 2 == 0) | take 5
```

### Exact access and traversal

```jq
get .user.address.city
has .config.features.dark_mode
pluck .orders .items .name
flatmap .orders | flatmap .items | map .name
```

`get` and `has` keep paths exact. `pluck` and `flatmap` are used when the query
should traverse through collections.

### Function arguments

```jq
pick .name .email
pick .name; .email
split "::"
truncate 20
```

Whitespace separates implicit arguments. Semicolons can be used when that makes
the boundary clearer.

### Object and array construction

```jq
{name, price}
{name, total: .price * .qty}
[.name, .price, .qty]
```

Object punning maps `{name}` to `{name: .name}` unless `name` is a bound value
from `let` or a lambda.

### Operators

```jq
.price + .tax
.price > 100
.active && .verified
.first ++ " " ++ .last
.email ?? "none"
```

Integer-only arithmetic stays exact for large integers.

### Conditionals, bindings, lambdas, interpolation

```jq
if .price > 100 then "expensive" else "cheap"
let total = .price * .qty in {name, total}
.items | map (x => {name: x.name, total: x.price * x.qty})
"Hello, ${.name}!"
```

## JSON Schema inference

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

## Built-in functions

### Collection

| Function | Description |
|---|---|
| `where expr` | Filter by predicate |
| `map expr` | Transform each element |
| `flatmap expr` | Transform each element and flatten one level |
| `sort_by expr` | Sort by derived key |
| `group_by expr` | Group into an object keyed by derived value |
| `unique` | Deduplicate while preserving order |
| `flatten` | Flatten nested arrays one level |
| `reverse` | Reverse order |

### Slicing and aggregation

| Function | Description |
|---|---|
| `first` | First element |
| `last` | Last element |
| `take n` | First `n` elements |
| `skip n` | Drop first `n` elements |
| `count`, `length` | Collection length |
| `sum` | Sum numbers |
| `min` | Minimum value |
| `max` | Maximum value |
| `avg` | Average of numbers |

### Strings

| Function | Description |
|---|---|
| `lower` | ASCII lowercase |
| `upper` | ASCII uppercase |
| `trim` | Strip leading and trailing whitespace |
| `truncate n` | Keep first `n` bytes |
| `split s` | Split on substring |
| `join s` | Join collection with separator |

### Objects and paths

| Function | Description |
|---|---|
| `get path` | Exact lookup |
| `has path` | Boolean exact-path existence check |
| `pluck p1 p2 ...` | Explicit traversal across nested collections |
| `keys` | Object keys |
| `values` | Object values |
| `pick k1 k2` | Select fields |
| `omit k1 k2` | Remove fields |

### Types and generators

| Function | Description |
|---|---|
| `type` | Return the value type name |
| `to_string` | Convert value to string |
| `to_number` | Convert string/bool/number to number |
| `range` | Infinite sequence `0, 1, 2, ...` |
| `range n` | Sequence `[0, n)` |
| `range m n` | Sequence `[m, n)` |

## Differences from jq

jq is mature, widely installed, and much more complete. Use jq when you need jq
compatibility, jq modules, its full CLI surface, or established streaming modes.

`jbq` makes different language choices:

| Area | jq | jbq |
|---|---|---|
| Filtering | `select(.price > 100)` | `where .price > 100` |
| Mapping | `map(.name)` | `map .name` |
| Iteration | Generator syntax such as `.items[]` | Named collection functions |
| Object construction | `{name: .name, price: .price}` | `{name, price}` |
| Missing fields | Often propagate as `null` | Error by default, `?` for optional access |
| Traversal | `.orders[].items[].name` | `pluck .orders .items .name` |
| String concatenation | `+` | `++` |
| JSON Schema | Not built in | `--schema` |

## Execution model

Collection operations such as `where`, `map`, `take`, `skip`, `unique`, and
`flatten` compose into a lazy pipeline. Elements flow through the whole pipeline
one at a time, and operations such as `take` can stop evaluation early.

```jq
.items | where .price > 100 | map .name | take 5
```

Aggregations such as `sum`, `avg`, and `count` fold over the pipeline without
building an intermediate collection. Operations that must see all values, such
as `sort_by`, `group_by`, `reverse`, and `last`, realize the collection first.

For compatible top-level array queries, input can be streamed from simdjson into
that pipeline instead of building the whole array up front.

## Development

```bash
make build        # build
make test         # run tests
make dev          # watch mode
make fmt          # format code
make clean        # clean build artifacts
```

## Architecture

```text
Input -> simdjson native parser -> Value.t / Value.Seq -> Lexer -> Parser -> Interpreter -> JSON output
```

Written in OCaml. Licensed under MIT.
