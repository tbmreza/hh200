# hh200 grammar (`.hhs`)

An hh200 script is a sequence of *call items*; each describes an HTTP request
and, optionally, the response to expect.

```
script    := call_item (newline+ call_item)*
call_item := [name | deps "then" name] request [response]
request   := [method] url newline [headers] [squares] [body] newline
response  := ["HTTP" codes newline] [headers] [squares] [body] newline
```

- `method` is `GET POST PUT DELETE PATCH OPTIONS HEAD TRACE CONNECT`
  (case-insensitive), default `GET`.
- `codes` is a list of expected status codes; default `200`.
- `headers` are `Key: value` lines.
- `body` is a JSON `{ ... }` object.
- Request `squares`: `[Configs] [Query] [Form]|[FormParams]
  [MultipartFormData] [Cookies]`.
- Response `squares`: `[Captures] [Asserts]`.

## Expressions (BEL)

Used in `{{ ... }}` interpolation, `[Captures]` values, and `[Asserts]` lines.

### Operators (highest binding first)

| Power | Operators | Assoc |
|---|---|---|
| 20 | `*` `/` | left |
| 10 | `+` `-` | left |
| 5  | `==` `!=` `<=` `>=` | left |

Prefix: `-` (negate), `debug`, `jsonpath`. Postfix: `copy`.
Special: `header "X" exists`, `header "X" not exists`.

### Literals

numbers, `"strings"`, `true`, `false`, identifiers. (`null`, JSON arrays,
and objects are values produced by `jsonpath` and captures, not literals.)

### Built-ins

- `jsonpath "$.x"` — first JSONPath match in the response body, else `null`.
- `debug <expr>` — print and mark the assertion passed.
- `<expr> copy` — copy the result to the clipboard.
- `header "X" exists` / `not exists` — response-header presence.

### Planned (not yet wired into the evaluator)

- `today()`, `year()`, `dayOfMonth()` — calendar date helpers.
- `timestamp()`, `duration()`, `dateIso()` — time helpers.
- `randInt(lo, hi)` — random integer in range.
- `size(s)`, `matches(s, re)` — string length and regex match.
- `loremChars(n)` — filler text of `n` characters.

## Examples

```hhs
GET http://httpbin.org/anything
HTTP 200
```

```hhs
GET http://127.0.0.1:9999/api/echo
Accept: application/json
X-Custom-Header: my-value
HTTP 200
```

```hhs
POST http://localhost:8080/echo
{
  "foo": "bar",
  "baz": 123
}
HTTP 200
[Captures]
TOK: Bearer ey{{100 * 24}}
[Asserts]
>debug TOK
```
