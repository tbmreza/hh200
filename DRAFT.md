# INDUSTRY SPEC SOURCES
## Syntax https://hh200-docs.pages.dev/#structure
- https://hurl.dev/docs/request.html#structure
## Cookies
- https://www.rfc-editor.org/rfc/rfc6265.html "HTTP State Management Mechanism"
- https://curl.se/docs/http-cookies.html      "curl HTTP Cookies"


# DESIGN
## General
- test; ltt main loop; lsp; mcp
- "Connection: close" header sent by default; close the connection after the current request/response pair. httpie sends Connection: close header by default, curl doesn't
- incremental parsing is non-goal if not very cheap
- use shelltestrunner for golden integration tests. bats-core looks too fine grained
- live: full svelte app that sends pause/resume/stop and receives SSE

oftenBodyless :: UppercaseString -> Bool
oftenBodyless (UppercaseString s) = elem s ["GET", "HEAD", "OPTIONS", "TRACE"]
```
git reset --soft HEAD~<number of commits>  # then commit

@.headers  $.headers  %.params.start
HH200_SQLITE=/home/tbmreza/gh/hh200/app.db stack run -- ../examples/alpha.hhs
```
Set-Cookie attribute enum, implementing Secure attr "Sends cookie only over HTTPS." https + Secure cookie attr
http(s) secure
    user story: distinct tls https (https://www.stackage.org/package/http-client-tls) and insecure http (https://hackage-content.haskell.org/package/http-client-0.7.19/docs/Network-HTTP-Client.html#g:4).
    in curl, tls connection may be determined by specified url, not headers

tcpretrans.py
libbpf-bootstrap has a running network example in tc.c
~/go/bin/lazysql --version


# SURPRISES
Some nuggets that are less than mundane when I learned them.
- Haskell std lib trace doesn't print if the last value is unused, and doesn't print at all on panicking path.
- Show in haskell is not intended to be overriden. https://stackoverflow.com/q/9288883
- server vs remote ports? remote == client


# STASH

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

POST https://host.com/v2/login
GET http://localhost:9999/echo.php
[Captures]
RELAY: TOKEN  # hurl uses colon for Captures


POST https://host.com/login.php
{ "username": "testing1", "password": "paragon" }
HTTP 200
[Captures]
NNN: 9
TOKEN: jsonpath "$.jwt"

POST  https://host.com/sfa2/web/index.php?r=delivery-order%2Fcreate-order
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
> jsonpath "$.data.method" == request method
# jsonpath "$.data.name" == "alice"
# implicit status == 200 assertion at the end

# goal on $ @ %
use  jsonpath "$." for response body as it's the norm
then jsonpath "%." can be used for all other response fields
requests are statically checked, so it make sense in this block that request is treated "natively"


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
