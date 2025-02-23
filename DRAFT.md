# hh200
```
summary:      statically checked dsl for testing http servers
extensions:   .hhs (script), 
usecases      stress testing (massive parallelism, pretty execution reports),
  (features): quick call (interpreter, user profile),
              programming assistant (lsp, symbolic executor)
others:       can read a subset of hurl


hh200 is an objectification of a software person specializing in HTTP systems testing.
Any number of times in a day, the thought "Let me hit this URL with such params real quick," forms in their brain.
Perhaps, after analyzing the insights they have further gathered, "Let's see if this web service is going to endure the test scenario that I have devised."

hh200 the language is a modest tool for expressing _what HTTP calls to make and how_.
It is not innovative by enabling what weren't possible with general purpose languages before. Also see hurl, who's been the inspiration for this project.
Nevertheless, nothing should prevent this tool from being used like a full-fledged programming language (examples/fibonacci.hhs, examples/factorial.hhs).

This repo is home to the following components:

-  The Hh200 Instruction Set specification document.
  ...implemented by...
- `.hhs` script frontend reference implementation (in haskell using conventional parser generators).
  ...provides input to...
- Hh200 compiler to core Erlang, wrapped with familiar package manager interface `hh200`.


"what calls to make and how" building-blocks:
lexer/parser with source-location error
parallel/concurrency as provided by haskell, core erlang
compile to core erlang and programming in it
abstract interpreter or symbolic executor


```



## cli
flags: hh200 hello.hhs --verbose, --no-cache, --version
subcommands: hh200 check hello.hhs, build-portable, format, parse curl.txt, 
env: SLUG=100 hh200 hello.hhs

## syntax
- evaluate expr with {{ }} like hurl
- unless replaces previous expression with arms
- scoped variables; let stmt that isn't followed by indentation is global
- for loop with let-until-step
- first class callables
- hurl doesn't have syntax for output file, only cli option https://github.com/Orange-OpenSource/hurl/discussions/2078
- statement is a tree that can have unevaled expressions in its leaf

POST https://httpbin.org/anything?page=2&lim={{LIMIT}} write out.png
{ "username": "admin", "id": 8 } unless
    isStaging -> { "username": "admin", "id": 79 }
    isProd    -> { "username": "admin", "id": 100 }

let httpBadRequest = 400
    { "statusCode": {{httpBadRequest}} }

let i = 0
    POST https://httpbin.org/anything?page=2&lim=10
    { "key": {{i}} }
until 4 step 1


let url = https://httpbin.org/anything?page=2&lim=10
let method = GET

callable login =  # captures visible variables
    { "body": 12 }

{{login}}


callable f =
    POST http://localhost
    { "payload": 12 }
    return response.data.token

let token = {{f}}

callable g = copy f with
    body = {}
    method = GET

mut f with
    method = PATCH

### hh200 check hello.hhs > hello.hhsm
parse :: filepath -> ast

### hh200 run hello.hhsm
eval :: ast -> env -> hhsm

## hhsm iset
goal: assert a property of an http response
instructions: v, h, c, u (resource)  i, o, r, s (action)
    <!-- IV: init verb get -->
    <!-- IC: init expect_code -->
    RV: revert verb
    RH: revert headers
    RC: revert expect_code
    OV: override verb (String)
    OC: override expect_code (Int)
    OJ: override json_string (String)
    JB: screen stomach, set body
    SU: set parametrized_url (String)
    SH: override a header (Map)
    X : run

type HttpMethod = String
mut VM {
    parametrized_url,  String
    expect_code,  Int
    verb,  String
    headers,  Tbl
    json_string,  String
    prog: [Stmt]
}

get, { "body": 12 }, headers, parametrized_url
status_code, jsonpath

### example program
....

```
init verb get
init expect_code 200

set parametrized_url
run  vm.json_string <- response.body, vm.expect_code == response.code  verb url expect_code

override verb
set parametrized_url
override expect_code
run  vm.json_string <- response.body, vm.expect_code == response.code





override verb
set parametrized_url
override json_string
set body
run  vm.expect_code == response.code





```
## Running cabal test
cd dev-server
php -f router.php -S localhost:9999

## Acknowledgments
- https://github.com/glguy/toml-parser (ISC license)



### Parser anew recipe

stack new ...
dependencies add array
    stack test
L.x starter from https://github.com/haskell/alex/blob/master/examples/Tokens_posn.x
    alex ...
    use alexScanTokens :: String -> [token]
P.y starter from https://github.com/haskell/alex/blob/master/examples/tiny.y
    happy ... --ghc
    parse $ alexScanTokens "let k"

# ??:
# GET, POST releases-65rb.pages.dev/state  { "rgrid": "v1.1.0" }
# so in appium rgrid we can:
# # download-apk.hhs
# GET releases-65rb.pages.dev/state  url="https://mobile-apps.paragoncorp.com/nova/v1.1.0.apk"  https://mobile-apps.paracorpgroup.com/nova/rc/nova%20v1.1.0-rc-sprint12.6.apk
