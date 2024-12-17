# hh200
```
summary: an imperative language that has first support for http requests
extensions: .hhs (script), .hhsm (machine code), .hhbc (machine code binary)
features: formatter, interpreted or standalone bin,
    parse/export standard curl invocation, smart caching, some compatibility with hurl, 
    "declare header for every requests",
lang: for-loops, if-conditionals, process.env, parallel clients,
```



## cli
flags: hh200 hello.hhs --verbose, --no-cache, --version
subcommands: hh200 check hello.hhs, build-portable, format, parse curl.txt, 
env: SLUG=100 hh200 hello.hhs

### hh200 check hello.hhs > hello.hhsm
parse :: filepath -> ast

### hh200 run hello.hhsm
eval :: ast -> env -> hhsm

## hhsm iset
goal: assert a property of an http response
instructions: v, h, c, u (resource)  i, o, r, s (action)
    IV: init verb get
    IC: init expect_code
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

### example hhsm program
http://localhost:8787/product/1222/first


POST http://localhost:8787/other
HTTP 201

POST http://localhost:8787/other
{ "data": 12 }
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
