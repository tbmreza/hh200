# PRIMARY (OR INDUSTRY) SPEC SOURCES
## Syntax
- https://hurl.dev/docs/request.html#structure
## Cookies
- https://www.rfc-editor.org/rfc/rfc6265.html "HTTP State Management Mechanism"
- https://curl.se/docs/http-cookies.html      "curl HTTP Cookies"


# Design decisions
## General
- "Connection: close" header sent by default; close the connection after the current request/response pair.

# DONE LIST
all protocols/specs about cookies
server vs remote ports? remote == client
httpie sends Connection: close header by default, curl doesn't
plan a self-maintaining fork of httpLbs: do sync using gh actions
Set-Cookie attribute enum, implementing Secure attr "Sends cookie only over HTTPS."
http(s) secure
    user story: distinct tls https (https://www.stackage.org/package/http-client-tls) and insecure http (https://hackage-content.haskell.org/package/http-client-0.7.19/docs/Network-HTTP-Client.html#g:4).
    in curl, tls connection is determined by specified url, not headers (unverified chatgpt)
https + Secure cookie attr


# STASH
`ls ~/.local/bin/` checks if `stack install` succeed.
simple but useful actual script
    release date of host's linux kernel version  uname -r
    GET https://kernel.org/releases.json
    [Captures]
    KERNEL_VER: "6.6"  # ??: uname -r  parsing
    # Capture the object that contains the string KERNEL_VER
    [Asserts]
    > (jsonpath "$.data.timestamp") has KERNEL_VER  # has or contains ??depends on CEL or hurl
    > eyeball KERNEL_VER (jsonpath "$.data.timestamp") "date"
    # eyeball needle haystack dfs_else_bubble
thread-based parallelism we name shotgun: based on async + QSemN
    plotter: data point (N, %)
counter-example presentation based on mvar
duration-bound virtual user we name rps (in reference to locust RPS)
whether DataPoint should generalize for all modes of Execution with pro cons
after above three, decide if doing 1 mill parallel requests is already in sight


                    -- Asserts.
                    -- nice to have bel lines statically checked regardless of the outside
                    -- world but full evaluation of bel requires outside world results.
                    --
                    -- a line doesn't mutate the Env.
                    --
                    -- world's and spec's status matching

    the mainline branch of the linux kernel last released on (isodate), the date when "event in past <year>"
    ??: pass on isodate to wiki on this day
https://en.wikipedia.org/w/rest.php/v1/search/page?q=earth&limit=1
https://en.wikipedia.org/wiki/November_5
GET http://localhost:8080/get
    {
    "releases": [
        {
            "moniker": "mainline",
            "version": "6.18-rc5",
            "iseol": false,
            "released": {
                "timestamp": 1762729819,
                "isodate": "2025-11-09"
            },
what to do about: Special-Use Domain Names, webdav semantics,

## Syntax ideas

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

[Asserts]
> jsonpath "$.json.kkk" == "vvv"

POST https://bivi-backend.pti-cosmetics.com/v2/login
GET http://localhost:9999/echo.php
[Captures]
RELAY: TOKEN


POST https://sfa-api-testing.pti-cosmetics.com/login.php
{ "username": "testing1", "password": "paragon" }
HTTP 200
[Captures]
NNN: 9
TOKEN: jsonpath "$.jwt"

POST  https://sfa.pti-cosmetics.com/sfa2/web/index.php?r=delivery-order%2Fcreate-order
jwt: eyabcd98
HTTP 203


-- matches("1234", "[0-9-]+")    "1234".matches("[0-9-]+")
-- size("") == 0
-- timestamp("") - timestamp("") = duration("")
-- randInt(lo, hi)
-- today() == dateIsoDate() dateIsoDateTime() dateIsoDateTimeZone() == dateIso()



[Captures]
BT = `I am {2 + 2} years old`
ID = $.data.id
START = today()
Q_STR = "salt and pepper"
[Asserts]
> 12 != 120
> false




POST http://localhost:9999/echo.php
{ "data": "ck" }
HTTP 200
[Captures]
Q_STR = "salt and pepper"
ID = $.data.id
START =   today()
[Asserts]
> 12 == 12  # early return
# jsonpath "$.data.name" == "alice"
# implicit status == 200 assertion at the end

POST http://localhost:9999/echo.php
{ "data": "ck" }
HTTP 200
[Captures]
Q_STR = "salt and pepper"
ID = $.data.id
START =   today()
[Asserts]
> 12 == 12  # early return
# jsonpath "$.data.name" == "alice"
# implicit status == 200 assertion at the end

POST http://localhost:9999/{{START}}&q={{Q_STR}}
{ "data": "a" }
HTTP 404

POST  http://localhost:9999/echo.php
{ "kkk": "vvv" }
HTTP 200
[Captures]
NNN: 9
[Asserts]
> true
