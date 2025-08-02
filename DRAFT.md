what to do about:
- Special-Use Domain Names

# hh200
```
summary:      statically-checked dsl for testing http servers
extensions:   .hhs (script), .etf (external term format)
usecases      stress testing (massive parallelism, pretty execution reports),
  (features): quick call (interpreter, user profile),
              programming assistant (lsp, symbolic executor)
others:       can read a subset of hurl

## Parser
Alex + Happy. monarch for syntax highlighting (maybe replace Alex if it makes sense).

## Finding first counter-example fast
### Flow interpreter
### Web UI

By default, hh200 opens the web browser for you to view the test execution live status and results.
hh200 the language is designed to be self-contained in the semantics that it can express, which should
allow the status dashboard to be a strictly read-only UI. We think that a dashboard that can control
the runtime deemphasizes even if slightly the urgency of a full DSL for the task.

As a bonus from this design decision, we prevent a class of test execution non-determinism.
[locust] is a popular existing tool whose web UI might inspire you to think otherwise, and you should
feel free to implement an "Edit" button in the dashboard if you find any use for that.

[locust]: https://locust.io



hh200 is a tool for testing HTTP servers. From "Let me hit this URL with such params
real quick," to "Let's see if this web service is enduring such & such test scenario",
hh200 was born for such developer occurrences.

hh200 the language expresses _what HTTP calls to make and how_.
It is not innovative by enabling what weren't possible with general purpose languages before.
Also see the state of hurl, which has been the inspiration for this project.

## Erlang
$ erl
Erlang/OTP 25 [erts-13.1.5] [source] [64-bit] [smp:20:20] [ds:20:20:10] [async-threads:1] [jit:ns]
Eshell V13.1.5

Erlang/OTP 27 [erts-15.2.2] [source] [64-bit] [smp:20:20] [ds:20:20:10] [async-threads:1] [jit:ns]
Eshell V15.2.2


"what calls to make and how" building-blocks:
lexer/parser with source-location error
parallel/concurrency as provided by haskell, erlang
abstract interpreter or symbolic executor


strategy: how to call callables
    probably a selling point to have it hotreloading erlang-style
callable: what to call
    partial and reads from environment
    { method, url }

"pass json-parsed response to later callables"

parse to erlang Abstract Format
execute a callable in a process
    
update context/environment from a process
    client-server app
    a dictionary<key, JsonString>  as rebar3 project: mcontext dictionary
distribute erlang's runtime
    erlang Release https://www.erlang.org/doc/system/release_structure
    as cemerlang/ with makefile: .core building blocks

{get,"http://localhost:9999/hello"}  -- set callable
{http,404}                           -- httpc


{get,{"http://localhost:9999/user?search=~a",["reza"]}}  -- interpolation, capture
{http,200}
{capture,{reza_id,21}}

{delete,{"http://localhost:9999/user/{{reza_id}}",{ctx}}}  -- read ctx
{http,200}

            % scan :: String -> [Token]
            % scan _ = do ... erl_scan:string ... return []
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

callable login =
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


Release 0.0.0-proto
- etf encoder
- OTP 27 requirement

```
erl -compile interpreter.erl && erl -run interpreter
erl -run interpreter
```

{patch, "http://localhost:9999/ls"}
{http,404}

{post, "https://bivi-backend-dev.paradev.io/v2/login"}
{json, #{username => "wardah.21", password => "ptiuser1234"}}
{http,404}





{patch, "http://localhost:9999/z"}
{json, #{username => <<"wardah.21">>, password => "ptiuser1234"}}
{http,404}



curl -X POST https://bivi-backend-staging.paradev.io/v2/login \
     -H "Content-Type: application/json" \
     -d "$(head -c 1048576 </dev/zero | tr '\0' 'a')"  


curl -X POST -H "Content-Type: application/json" -d '{"data": "'$(python -c "print('x' * 10**6)")'"}' https://bivi-backend-staging.paradev.io/v2/login
curl -X POST -H "Content-Type: application/json" --data-binary @payload.json https://bivi-backend-staging.paradev.io/v2/login

"/home/tbmreza/gh/hh200/building-blocks/etf/output.etf"

??:
suite manager:
  watch ir changes
  view reports
  parse config values

data Policy = Policy {
    maxReruns :: Maybe Int,
    maxRetriesPerCall :: Maybe Int,
    timeMillisPerCall :: Maybe Int
    } deriving (Eq, Show, Generic)

https://hurl.dev/docs/request.html#structure




STATIC PHASE

On sight, if outputs exist and warn/error

callable { deps: [name], name }

Provision how many parallel clients are needed and which callables go to
which clients
	If then-prefixes is empty, spawn new client. The callable's dependants shall reuse that same client
	clients<name, client_id>
	If callable has deps, find clients.dep


[callable]



PRE SWEEP or exit clients

At least 1 callable has empty then-prefixes

Assert we have write permissions at output paths

[callable]
worker(init())


FIRST SWEEP

Callable is called if Acc.callstack contains callable.deps
callable { been_called: true, err_stack: [] }

If a callable has err, re-sweep with new env/world (fs, network)


SECOND SWEEP

Re-evaluate an unsuccessful callable before sweeping

Send exit to clients

## Erlang runtime (abandoned idea, unproductive overhead to our goal that is getting to first counter-example fast instead of being fault tolerant)
### Development
The shell might interfere with how we interact with `/bin/erl_call` sometimes, for example with the
out-of-context message "erl_call:.:{N}: not enough arguments". Use another shell or look closer to
what your shell does every step of the way.


- [X] LR parser
- [ ] agent parallelism phase 1 (4 agents)
    - file reading built into the syntax
    - counter-example and summary printing: Duration; Request string repr; 
- [ ] agent parallelism benchmark
- [ ] `stack test -- localhost.hhs # opens localhost:44200`  "haskell serve real time data to localhost"
- [ ] `stack test -- localhost.hhs --no-browser` cli arg
- [ ] `hh200 prgn-nova-regression.hhs` debian packaging

## old Makefile:
reg:
	cabal test
	cabal run -j4 hh200 -- --version


# test:
# 	hh200 compile --io examples/hello.hhs [--policies specs.aimfor.toml]        # prints warnings to std err, or test on the outside world (dynamically-checked programming style) [override policies]
# 	hh200 compile examples/index.hhs [--policies specs.aimfor.toml] > out.hhsm  # prints warnings to std err [override policies], or hhsm source to std out (indirectable)
# 	hh200 interactive examples/hhsm/hello.hhsm                                  # asks confirmation before every side-effects

locust:
	# 901. ...905 (unacceptable perf) ...900 (acceptable) return 905 (less than aim)
	# the number at which system under test falls off acceptable performance
	aimfor [--config config.aimfor.toml] -n 1000 --specs specs.aimfor.toml smoke.hhs  # hh200 build examples/hello.hhs --policies specs.aimfor.toml  where build depends on parallelization strat

	# properties of interest: "System under test can handle N parallel users."
	# "Number of users is *the only factor* of any under-achievements." is a non-property as it's impossible to say about any unpure systems.
	#
	# To that goal, we increase a stride at a time our number of parallel users until we get unacceptable* perf.
	#
	# config.aimfor.toml: Mechanisms that shouldn't affect our property check.
	# stride = 0  # handled as {n div 10} by runtime
	# all_stats = 2  # "open|forget|quiet"
	#
	# *specs.aimfor.toml: worst acceptable execution stat of serving 1 user defined in numbers:
	# max_reruns = 2
	# max_retries_per_call = 2
	# time_millis_per_call = 60_000
	# # example: index.hhs contains C1 and C2 (where C2 is contrived to always fail) calls. Sequentially, C1 succeed and C2 failed the first time.
	# # Then, C2 is retried {max_retries_per_call} times.
	# # Then, the whole index.hhs is run {max_reruns} more times.

help:
	cabal build -j4
	dist-newstyle/build/x86_64-linux/ghc-9.8.2/hh200-0.1.0.0/x/hh200/build/hh200/hh200 --help

download:
	cabal run -j4 hh200 -- --hello examples/debug.hhs download https://httpbin.org/latest.apk

run:
	cabal run -j4 hh200 -- examples/debug.hhs

# # dist-newstyle/build/x86_64-linux/ghc-9.4.7/...
# dist-newstyle/build/x86_64-linux/ghc-9.8.2/hh200-0.1.0.0/x/hh200/build/hh200/hh200 --hello examples/debug.hhs
