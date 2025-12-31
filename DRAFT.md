# PRIMARY (OR INDUSTRY) SPEC SOURCES
## Syntax
- https://hh200-docs.pages.dev/#structure  https://hurl.dev/docs/request.html#structure
```
@.headers  $.headers  %.params.start
```
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
`ls ~/.local/bin/` checks if `stack install` succeed.


# STASH
oftenBodyless :: UppercaseString -> Bool  -- starting point for webdav or lints
oftenBodyless (UppercaseString s) = elem s ["GET", "HEAD", "OPTIONS", "TRACE"]


Debug traceStack and stack run --profile -- ../examples/asserts_block.hhs +RTS -xc
[ ScriptStart 1
, ItemStart ""
, HttpError "HttpExceptionRequest Request {\n  host = \"example.io\"
                                           \n  port = 443
                                           \n  secure = True
                                           \n  requestHeaders = [(\"authorization\",\"<REDACTED>\")]
                                           \n  path = \"/v2/target-template\"
                                           \n  queryString          = \"?project_id=24& year=2025&month=12&salesarea=5\"
                                           \n  method               = \"GET\"
                                           \n  proxy                = Nothing
                                           \n  rawBody              = False
                                           \n  redirectCount        = 10
                                           \n  responseTimeout      = ResponseTimeoutDefault
                                           \n  requestVersion       = HTTP/1.1
                                           \n  proxySecureMode      = ProxySecureWithConnect
                                           \n}
                                \n (InternalException
                                       (HandshakeFailed
                                           (Error_Protocol \"certificate rejected: [NameMismatch \\\"example.io\\\"]\"
                                            CertificateUnknown)))"
]

hh200-docs page for hh200d
POST http://localhost:9999/echo.php
{ "foo": "bar", "baz": 123 }
HTTP 200
[Asserts]
>debug $.key

Based on the "MAIN THESIS" comments in app/Main.hs and the structure I verified in src/Hh200/Cli.hs, here are the questions an academic reviewer might have, focused on the
  claims and methodology implied by your code:

   1. Methodological Novelty: "You describe a 3-step pipeline (Scanner $\rightarrow$ Execution $\rightarrow$ Presentation) as the core thesis. How does this architecture differ
      conceptually from existing HTTP client test runners? Is the innovation in the coupling of static analysis (Scanner) with runtime verification?" >It's HTTP testing expert opinions enforced via a test runner and an LSP.
   2. Formal Semantics: "The Scanner.analyze step is described as providing 'linter hints' and 'static checks'. Does the DSL have a formal specification or type system that
      guarantees certain properties before execution? What class of errors can be caught statically versus dynamically?" >We'll prove certain protocols/idioms disobedience are checkable statically.
   3. Correctness vs. Performance: "The thesis lists 'testRps' (requests per second) and 'testShotgun' (concurrency) alongside correctness checks ('status codes mismatch'). Is
      the primary contribution of hh200 a correctness verifier or a load testing tool? How do you balance the trade-off between the overhead of correctness assertions and the
      speed required for load generation?" >The trade-off is in the hands of test case designer. The more complex the single script is, the lower should we anticipate the maximum capacity is. We doubt there's immense reward in defining what's complex and benchmarking it, though.
   4. Deterministic "Reality": "The code mentions handling 'thread cancelled' and 'offline' states under the 'Reality' section. How does your execution model ensure
      reproducibility when dealing with these non-deterministic network conditions? Do you simulate these states or rely on real-world flakiness?" >We extend the pens of test case designers: they want real-world results, they get real-world results.
   5. Compliance Standards: "You mention checking for 'HTTP idioms' and 'WebDAV status code misuse'. Which specific RFCs or standards are you strictly adhering to? Are these
      checks opinionated/heuristic or strictly standards-compliant?" >We'll show how the standards map to our tests.

  These questions target the gap between what the code does (runs tests) and why it is structured this way (the "Thesis").
   * Figure 1: `hh200` System Architecture Overview (Illustrating the 3-step pipeline: Scanner, Execution, Presentation)
   * Figure 2: Static Analysis Workflow for HTTP Idioms (Detailing the process of Scanner.analyze)
   * Figure 3: Example of Detected HTTP Protocol Violation (Linter Hint)
   * Figure 4: Request Latency Distribution under Concurrent Load (Derived from testShotgun results)
   * Figure 5: Throughput (RPS) vs. Number of Concurrent Workers (Derived from testShotgun or testRps results)
   * Figure 6: HTTP Status Code Mismatch Detection Example
   * Figure 7: Time-series Plot of Test Metrics (e.g., Latency, Errors) over Duration (Output from Graph.plot or similar)


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

POST https://host.com/v2/login
GET http://localhost:9999/echo.php
[Captures]
RELAY: TOKEN


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
