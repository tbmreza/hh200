# hh200
summary: an imperative language that has first support for http requests
extensions: .hhs, .hhir, 
features: formatter, interpreted or standalone bin,
    parse/export standard curl invocation, smart caching, some compatibility with hurl, 
    "declare header for every requests",
lang: for-loops, if-conditionals, process.env, parallel clients,



## cli
flags: hh200 hello.hhs --verbose, --no-cache, --version
subcommands: hh200 check hello.hhs, build-portable, format, parse curl.txt, 
env: SLUG=100 hh200 hello.hhs

### hh200 check hello.hhs --dump > hello.hhir
parse :: filepath -> ast

### hh200 run hello.hhir
eval :: ast -> env -> hhir

## hhir
goal: assert a property of an http response
instructions:
    init verb (get),
    init headers,
    override verb,
    override a header,
    override expect_code,
    revert headers,
    set parametrized_url,
    run,

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

### example hhir program
http://localhost:8787/product/1222/first


POST http://localhost:8787/other
HTTP 201
....

```
init verb (get)
init expect_code 200

set parametrized_url
run  vm.json_string <- response.body, vm.expect_code == response.code  verb url expect_code

override verb
set parametrized_url
override expect_code
run  vm.json_string <- response.body, vm.expect_code == response.code





```


### hh200 build-portable hello.hhir -o hello.exe
